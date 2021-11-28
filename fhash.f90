#ifdef __DO_NOT_PREPROCESS_DOC__
! Hash table implementation imitating to GCC STL (with singly linked list).
! DO NOT COMPILE THIS TEMPLATE FILE DIRECTLY.
! Use a wrapper module and include this file instead, e.g. fhash_modules.f90.
! Remove is not implemented since not needed currently.
!
! #define                         | meaning
! --------------------------------+-----------------------------------------------------
! FHASH_NAME <Name>               | The name of the type of FHASH table.
!                                 |
! KEY_USE <use stmt>              | (optional) A use statement that is required to use
!                                 | a specific type as a key for the FHASH
! KEY_TYPE <typename>             | The type of the keys. May require KEY_USE to be
!                                 | accessible.
! KEYS_EQUAL_FUNC <function>      | (optional) function that returns whether two keys
!                                 | are equal. Defaults to `a == b` or `all(a == b)`,
!                                 | depending on whether the key is a scalar.
!                                 |
! VALUE_USE <use stmt>            | (optional) A use statement that is required to use
!                                 | a specific type as a value for the FHASH
! VALUE_TYPE <typename>           | The type of the values. May require VALUE_USE to be
!                                 | accessible.
!
! HASH_FUNC                       | (optional) hash function name. Defaults to 'hash'.
!                                 |
! VALUE_POINTER                   | (optional) If defined, the values are pointers.
! VALUE_ASSIGNMENT                | (internal) The assignment operator, do not set it
!                                 | anywhere, it is configured based on VALUE_POINTER
#endif

#ifdef __GFORTRAN__
#    define _FHASH_PASTE(a) a
#    define _FHASH_CONCAT(a,b) _FHASH_PASTE(a)b
#else
#    define _FHASH_PASTE(a,b) a ## b
#    define _FHASH_CONCAT(a,b) _FHASH_PASTE(a,b)
#endif
#define _FHASH_MODULE_NAME _FHASH_CONCAT(FHASH_NAME,_mod)
#define _FHASH_TYPE_NAME _FHASH_CONCAT(FHASH_NAME,_t)
#define _FHASH_TYPE_ITERATOR_NAME _FHASH_CONCAT(FHASH_NAME,_iter_t)
#define _FHASH_TYPE_KV_TYPE_NAME _FHASH_CONCAT(FHASH_NAME,_kv_t)
#define _FHASH_SORT_KV_NAME _FHASH_CONCAT(sort_,FHASH_NAME)

! For some bizar reason both gfortran-10 and ifort-2021.4 fail to compile, unless
! this function has a unique name for every time that this file is included:
#define _FHASH_COMPARE_AT_IDX _FHASH_CONCAT(fhash_type_compare__,FHASH_NAME)

#ifdef VALUE_POINTER
#   define VALUE_ASSIGNMENT =>
#else
#   define VALUE_ASSIGNMENT =
#endif

! Not all compilers implement finalization:
#if defined __GFORTRAN__ && __GNUC__ <= 5
#else
#   define _FHASH_FINAL_IS_IMPLEMENTED
#endif
#ifdef _FHASH_FINAL_IS_IMPLEMENTED
#   define _FHASH_FINAL_TYPEORCLASS type
#else
#   define _FHASH_FINAL_TYPEORCLASS class
#endif

module _FHASH_MODULE_NAME

#ifdef KEY_USE
   KEY_USE
#undef KEY_USE
#endif
#ifdef VALUE_USE
   VALUE_USE
#undef VALUE_USE
#endif
   use iso_fortran_env, only: int32, int64
   implicit none

   private

   public :: _FHASH_TYPE_NAME
   public :: _FHASH_TYPE_ITERATOR_NAME
   public :: _FHASH_TYPE_KV_TYPE_NAME
   public :: _FHASH_SORT_KV_NAME ! for convenience, because it's hard for the users to write a generic sort
   ! (that circumvents the compiler bugs when passing pointers to internal functions to `qsort`)

   type :: _FHASH_TYPE_KV_TYPE_NAME
      KEY_TYPE :: key
      VALUE_TYPE :: value
   end type

   type :: node_type
      type(_FHASH_TYPE_KV_TYPE_NAME), allocatable :: kv
      type(node_type), pointer :: next => null()

   contains
      ! Return the length of the linked list start from the current node.
      procedure, non_overridable :: node_depth

      ! No FINAL procedure here, because it would have to be recursive (at least
      ! implicitly, because it finalizes the 'next' pointer), and a recursive
      ! procedure is not performant.
      ! Fortunately this type is not public, and it gets deallocated when finalizing the fhash.
   end type

   type _FHASH_TYPE_NAME
      private

      integer :: n_keys = 0
      type(node_type), contiguous, pointer :: buckets(:) => null()

   contains
      ! Returns the number of buckets.
      procedure, non_overridable, public :: bucket_count

      ! Return the number of collisions.
      procedure, non_overridable, public :: n_collisions

      procedure, non_overridable, public :: reserve
      procedure, non_overridable, public :: resize

      ! Returns number of keys.
      procedure, non_overridable, public :: key_count

      procedure, non_overridable, public :: set

      ! Get the value at the given key.
      procedure, non_overridable, public :: get

#ifndef VALUE_POINTER
      generic :: get_ptr => get_ptr_or_autoval, get_ptr_or_null
      procedure, non_overridable, public :: get_ptr_or_null
      procedure, non_overridable, public :: get_ptr_or_autoval
#endif

      ! Remove the value with the given key.
      procedure, non_overridable, public :: remove

      ! Get the key/value pairs as a list:
      procedure, non_overridable, public :: as_list
      procedure, non_overridable, public :: as_sorted_list

      ! Return the accumalated storage size of an fhash, including the underlying pointers.
      ! Takes the bit size of a key-value pair as an argument.
      procedure, non_overridable, public :: deep_storage_size => fhash_deep_storage_size

      ! Clear all the allocated memory
      procedure, non_overridable, public :: clear
#ifdef _FHASH_FINAL_IS_IMPLEMENTED
      final :: clear_final
#endif
      generic, public :: assignment(=) => deepcopy_fhash
      procedure, non_overridable, private :: deepcopy_fhash

      procedure, non_overridable, private :: key2bucket
   end type

   type _FHASH_TYPE_ITERATOR_NAME
      private

      integer :: bucket_id
      type(node_type), pointer :: next_node => null()
      logical :: next_is_at_base
      type(node_type), pointer :: buckets_ptr(:) => null()

   contains
      procedure, non_overridable, public :: begin
      procedure, non_overridable, public :: has_next
      procedure, non_overridable, public :: next
   end type

   integer, parameter :: bucket_sizes(*) = [ &
         5, 11, 23, 47, 97, 199, 409, 823, 1741, 3469, 6949, 14033, &
         28411, 57557, 116731, 236897, 480881, 976369,1982627, 4026031, &
         8175383, 16601593, 33712729, 68460391, 139022417, 282312799, &
         573292817, 1164186217, 2147483647 &
   ]

   interface
      integer function compare_keys_i(a, b)
         import
         implicit none
         KEY_TYPE, intent(in) :: a, b
      end function
   end interface
   procedure(compare_keys_i), pointer :: global_compare_ptr => null()
   type(_FHASH_TYPE_KV_TYPE_NAME), pointer :: global_sorted_kv_list_ptr(:) => null()

   integer, parameter :: largest_int = merge(int64, int32, int64 > 0)

contains
   logical function keys_equal(a, b)
      KEY_TYPE, intent(in) :: a, b

      interface all
         procedure :: scalar_all
      end interface

#ifdef KEYS_EQUAL_FUNC
      keys_equal = KEYS_EQUAL_FUNC(a, b)
#else
      keys_equal = all(a == b)
#endif

   contains
      logical function scalar_all(scal)
         logical, intent(in) :: scal

         scalar_all = scal
      end function
   end function

   function bucket_count(this)
      class(_FHASH_TYPE_NAME), intent(in) :: this
      integer :: bucket_count

      if (.not. associated(this%buckets)) then
         bucket_count = 0
      else
         bucket_count = size(this%buckets)
      endif
   end function

   function n_collisions(this)
      class(_FHASH_TYPE_NAME), intent(in) :: this
      integer :: n_collisions

      integer :: i

      n_collisions = 0
      if (associated(this%buckets)) then
         do i = 1, size(this%buckets)
            n_collisions = n_collisions + node_depth(this%buckets(i)) - 1
         enddo
      endif
   end function

   recursive function node_depth(this) result(depth)
      class(node_type), intent(in) :: this
      integer :: depth

      if (.not. associated(this%next)) then
         depth = 1
      else
         depth = 1 + node_depth(this%next)
      endif
   end function

   impure elemental subroutine reserve(this, n_buckets)
      class(_FHASH_TYPE_NAME), intent(out) :: this
      integer, intent(in) :: n_buckets

      integer :: i
      integer, parameter :: n = size(bucket_sizes)

      call assert(bucket_sizes(2:) - bucket_sizes(:n-1) > 0, "PROGRAMMING ERROR: bucket_sizes should be strictly increasing")
      call assert(bucket_sizes(n) >= n_buckets, "Did not expect to need this many buckets.")

      do i = 1, n
         if (bucket_sizes(i) >= n_buckets) then
            allocate(this%buckets(bucket_sizes(i)))
            exit
         endif
      enddo
   end subroutine

   subroutine resize(this, new_n_buckets)
      ! Copy into a new hash. Delete the old hash during the copying to reduce the maximum memory consumption.
      class(_FHASH_TYPE_NAME), intent(inout) :: this
      integer, intent(in) :: new_n_buckets

      type(_FHASH_TYPE_NAME) :: new
      type(_FHASH_TYPE_ITERATOR_NAME) :: iter

      type(node_type), pointer :: prev
      logical :: dealloc_prev
      integer :: i, n

      if (.not. associated(this%buckets)) then
         call this%reserve(new_n_buckets)
         return
      endif

      call new%reserve(new_n_buckets)
      n = this%key_count()

      call iter%begin(this)
      do i = 1, n
         dealloc_prev = .not. iter%next_is_at_base
         prev => iter%next_node
         call move_into_fhash(new, prev%kv)
         call progress_to_next_node(iter)
         if (dealloc_prev) deallocate(prev)
      enddo
      call assert(n == new%key_count(), "INTERNAL ERROR: resize: inconsistent key count")

      this%n_keys = n
      deallocate(this%buckets)
      this%buckets => new%buckets
      new%buckets => null() ! to prevent finalization
   end subroutine

   impure elemental function key_count(this)
      class(_FHASH_TYPE_NAME), intent(in) :: this
      integer :: key_count

      key_count = this%n_keys
   end function

   subroutine set(this, key, value)
      class(_FHASH_TYPE_NAME), intent(inout) :: this
      KEY_TYPE, intent(in) :: key
      VALUE_TYPE, intent(in) :: value
      
      type(_FHASH_TYPE_KV_TYPE_NAME), allocatable :: kv

      call assert(associated(this%buckets), "set: fhash has not been initialized")

      allocate(kv)
      kv%key = key
      kv%value VALUE_ASSIGNMENT value
      call move_into_fhash(this, kv)
   end subroutine

   subroutine move_into_fhash(this, kv)
      class(_FHASH_TYPE_NAME), intent(inout) :: this
      type(_FHASH_TYPE_KV_TYPE_NAME), allocatable, intent(inout) :: kv

      integer :: bucket_id
      logical :: is_new

      call assert(associated(this%buckets), "INTERNAL ERROR: move_into_fhash: fhash has not been initialized")

      bucket_id = this%key2bucket(kv%key)
      call move_into_node_list(this%buckets(bucket_id), kv, is_new)

      if (is_new) this%n_keys = this%n_keys + 1
   end subroutine

   recursive subroutine move_into_node_list(this, kv, is_new)
      type(node_type), intent(inout) :: this
      type(_FHASH_TYPE_KV_TYPE_NAME), allocatable, intent(inout) :: kv
      logical, intent(out) :: is_new

      if (.not. allocated(this%kv)) then
         call move_alloc(kv, this%kv)
         is_new = .true.
      else if (keys_equal(this%kv%key, kv%key)) then
         this%kv%value VALUE_ASSIGNMENT kv%value
         is_new = .false.
      else
         if (.not. associated(this%next)) allocate(this%next)
         call move_into_node_list(this%next, kv, is_new)
      endif
   end subroutine

   subroutine get(this, key, value, success)
      class(_FHASH_TYPE_NAME), intent(in) :: this
      KEY_TYPE, intent(in) :: key
      VALUE_TYPE, intent(out) :: value
      logical, optional, intent(out) :: success

      integer :: bucket_id

      call assert(associated(this%buckets), "get: fhash has not been initialized")

      bucket_id = this%key2bucket(key)
      call node_get(this%buckets(bucket_id), key, value, success)
   end subroutine

   recursive subroutine node_get(this, key, value, success)
      ! If kv is not allocated, fail and return 0.
      ! If key is present and the same as the key passed in, return the value in kv.
      ! If next pointer is associated, delegate to it.
      ! Otherwise, fail and return 0.
      type(node_type), intent(in) :: this
      KEY_TYPE, intent(in) :: key
      VALUE_TYPE, intent(out) :: value
      logical, optional, intent(out) :: success

      if (.not. allocated(this%kv)) then
         ! Not found. (Initial node in the bucket not set)
         if (present(success)) success = .false.
      else if (keys_equal(this%kv%key, key)) then
         value VALUE_ASSIGNMENT this%kv%value
         if (present(success)) success = .true.
      elseif (.not. associated(this%next)) then
         if (present(success)) success = .false.
      else
         call node_get(this%next, key, value, success)
      endif
   end subroutine

#ifndef VALUE_POINTER
   function get_ptr_or_null(this, key) result(value)
      class(_FHASH_TYPE_NAME), intent(in) :: this
      KEY_TYPE, intent(in) :: key
      VALUE_TYPE, pointer :: value

      integer :: bucket_id
      type(node_type), pointer :: bucket

      call assert(associated(this%buckets), "get: fhash has not been initialized")

      bucket_id = this%key2bucket(key)
      call assert(1 <= bucket_id .and. bucket_id <= size(this%buckets), "get: fhash has not been initialized")
      bucket => this%buckets(bucket_id)

      value => node_get_ptr_or_null(bucket, key)
   end function

   recursive function node_get_ptr_or_null(this, key) result(value)
      type(node_type), target, intent(in) :: this
      KEY_TYPE, intent(in) :: key
      VALUE_TYPE, pointer :: value

      if (.not. allocated(this%kv)) then
         value => null()
      else if (keys_equal(this%kv%key, key)) then
         value => this%kv%value
      else if (.not. associated(this%next)) then
         value => null()
      else
         value => node_get_ptr_or_null(this%next, key)
      endif
   end function

   function get_ptr_or_autoval(this, key, autoval) result(value)
      class(_FHASH_TYPE_NAME), intent(inout) :: this
      KEY_TYPE, intent(in) :: key
      VALUE_TYPE, intent(in) :: autoval
      VALUE_TYPE, pointer :: value

      integer :: bucket_id
      type(node_type), pointer :: bucket
      logical :: is_new

      call assert(associated(this%buckets), "get: fhash has not been initialized")

      bucket_id = this%key2bucket(key)
      call assert(1 <= bucket_id .and. bucket_id <= size(this%buckets), "get: fhash has not been initialized")
      bucket => this%buckets(bucket_id)

      call node_get_ptr_or_autoval(bucket, key, value, is_new, autoval)
      if (is_new) this%n_keys = this%n_keys + 1
   end function

   recursive subroutine node_get_ptr_or_autoval(this, key, value, is_new, autoval)
      type(node_type), target, intent(inout) :: this
      KEY_TYPE, intent(in) :: key
      VALUE_TYPE, pointer, intent(out) :: value
      logical, intent(out) :: is_new
      VALUE_TYPE, intent(in) :: autoval

      if (.not. allocated(this%kv)) then
         allocate(this%kv)
         this%kv%key = key
         this%kv%value = autoval
         value => this%kv%value
         is_new = .true.
      else if (keys_equal(this%kv%key, key)) then
         value => this%kv%value
         is_new = .false.
      else if (.not. associated(this%next)) then
         allocate(this%next)
         allocate(this%next%kv)
         this%next%kv%key = key
         this%next%kv%value = autoval
         value => this%next%kv%value
         is_new = .true.
      else
         call node_get_ptr_or_autoval(this%next, key, value, is_new, autoval)
      endif
   end subroutine
#endif

   subroutine remove(this, key, success)
      class(_FHASH_TYPE_NAME), intent(inout) :: this
      KEY_TYPE, intent(in) :: key
      logical, optional, intent(out) :: success

      integer :: bucket_id
      logical ::  locSuccess
      type(node_type), pointer :: first, temp

      call assert(associated(this%buckets), "remove: fhash has not been initialized")

      bucket_id = this%key2bucket(key)
      first => this%buckets(bucket_id)

      if (.not. allocated(first%kv)) then
         locSuccess = .false.
      elseif (.not. keys_equal(first%kv%key, key)) then
         call node_remove(first, key, locSuccess)
      elseif (associated(first%next)) then
         call move_alloc(first%next%kv, first%kv)
         temp => first%next
         first%next => first%next%next
         deallocate(temp)
         locSuccess = .true.
      else
         deallocate(first%kv)
         locSuccess = .true.
      endif

      if (locSuccess) this%n_keys = this%n_keys - 1
      if (present(success)) success = locSuccess
   end subroutine

   recursive subroutine node_remove(last, key, success)
      ! If kv is not allocated, fail and return
      ! If key is present and node is first in bucket, set first node in bucket to
      !   the next node of first. Return success
      ! If key is present and the node is another member of the linked list, link the
      !   previous node's next node to this node's next node, deallocate this node,
      !   return success
      ! Otherwise, fail and return 0
      type(node_type), intent(inout) :: last
      KEY_TYPE, intent(in) :: key
      logical, intent(out) :: success

      type(node_type), pointer :: next

      next => last%next

      if (.not. allocated(next%kv)) then
         success = .false.
      else if (keys_equal(next%kv%key, key)) then
         last%next => next%next
         deallocate(next%kv)
         success = .true.
      else if (.not. associated(next%next)) then
         success = .false.
      else
         call node_remove(next, key, success)
      endif
   end subroutine

   subroutine as_list(this, kv_list)
      class(_FHASH_TYPE_NAME), target, intent(in) :: this
      type(_FHASH_TYPE_KV_TYPE_NAME), intent(out) :: kv_list(:)

      integer :: i, n
      type(_FHASH_TYPE_ITERATOR_NAME) :: iter

      n = this%key_count()
      call assert(size(kv_list) == n, "as_list: kv_list has a bad size")

      call iter%begin(this)
      do i = 1, n
         call iter%next(kv_list(i)%key, kv_list(i)%value)
      enddo
      call assert(.not. iter%has_next(), "INTERNAL ERROR: as_list: iterator has too many elements")
   end subroutine

   subroutine as_sorted_list(this, kv_list, compare)
      class(_FHASH_TYPE_NAME), target, intent(in) :: this
      type(_FHASH_TYPE_KV_TYPE_NAME), target, intent(out) :: kv_list(:)
      procedure(compare_keys_i) :: compare

      call this%as_list(kv_list)
      call _FHASH_SORT_KV_NAME(kv_list, compare)
   end subroutine

   subroutine _FHASH_SORT_KV_NAME(kv_list, compare)
      type(_FHASH_TYPE_KV_TYPE_NAME), target, intent(inout) :: kv_list(:)
      procedure(compare_keys_i) :: compare

      call assert(.not. (associated(global_compare_ptr) .or. associated(global_sorted_kv_list_ptr)), &
         "It looks like I am already sorting, and this is not thread-safe.")
      global_compare_ptr => compare
      global_sorted_kv_list_ptr => kv_list

      call permute(kv_list, sorting_perm())

      global_compare_ptr => null()
      global_sorted_kv_list_ptr => null()
   end subroutine

   subroutine permute(x, perm)
      ! Performs
      !     x = x(perm)
      ! but (i) this is more efficient, and (ii) ifort appears to put `x(perm)` on
      ! the stack before copying, causing a segfault for large arrays.
      use, intrinsic :: iso_c_binding, only: c_int
      use, intrinsic :: iso_fortran_env, only: int8, int16

      type(_FHASH_TYPE_KV_TYPE_NAME), intent(inout) :: x(:)
      integer(c_int), intent(in) :: perm(:)

      type(_FHASH_TYPE_KV_TYPE_NAME) :: temp
      integer :: i, n, j, jnew
      integer, parameter :: smallest_int = merge(int8, int16, int8 > 0)
      logical(smallest_int), allocatable :: done(:)

      call assert(size(x) ==  size(perm), "INTERNAL ERROR: permute: inconsistent sizes")
      n = size(x)

      allocate(done(n))
      done = .false._smallest_int
      do i = 1, n
         if (done(i)) cycle
         ! Follow the permutations, which form a cycle.
         ! (All KV copies could be replaced by a cheaper user-defined move function.)
         j = i
         temp = x(i)
         do
            jnew = perm(j)
            if (jnew == i) then
               x(j) = temp
            else
               x(j) = x(jnew)
            endif
            done(j) = .true._smallest_int
            j = jnew
            if (done(j)) exit
         enddo
      enddo
   end subroutine

   impure elemental subroutine deepcopy_fhash(lhs, rhs)
      class(_FHASH_TYPE_NAME), intent(out) :: lhs
      type(_FHASH_TYPE_NAME), intent(in) :: rhs

      integer :: i

      if (.not. associated(rhs%buckets)) return

      lhs%n_keys = rhs%n_keys
      allocate(lhs%buckets(size(rhs%buckets)))
      do i = 1, size(lhs%buckets)
         call deepcopy_node(rhs%buckets(i), lhs%buckets(i))
      enddo
   end subroutine

   recursive subroutine deepcopy_node(this, copy)
      class(node_type), intent(in) :: this
      type(node_type), intent(out) :: copy

      if (.not. allocated(this%kv)) then
         call assert(.not. associated(this%next), 'internal error: node has a "next" pointer, but it''s kv pair has not been set')
      else
         allocate(copy%kv, source=this%kv)
      endif

      if (associated(this%next)) then
         allocate(copy%next)
         call deepcopy_node(this%next, copy%next)
      endif
   end subroutine

   impure elemental integer(largest_int) function fhash_deep_storage_size(this, kv_ss) result(s)
      class(_FHASH_TYPE_NAME), intent(in) :: this
      integer, optional, intent(in) :: kv_ss

      integer :: i
      integer(largest_int) :: my_kv_ss
      type(_FHASH_TYPE_KV_TYPE_NAME) :: dummy

      if (present(kv_ss)) then
         my_kv_ss = int(kv_ss, kind=largest_int)
      else
         my_kv_ss = storage_size(dummy, kind=largest_int)
      endif

      s = storage_size(this, kind=largest_int)
      if (associated(this%buckets)) then
         do i = 1, size(this%buckets)
            s = s + node_deep_storage_size(this%buckets(i), my_kv_ss)
         enddo
      endif
   end function

   recursive integer(largest_int) function node_deep_storage_size(node, kv_ss) result(s)
      type(node_type), intent(in) :: node
      integer(largest_int), intent(in) :: kv_ss

      s = storage_size(node, kind=largest_int)
      if (allocated(node%kv)) s = s + kv_ss
      if (associated(node%next)) s = s + node_deep_storage_size(node%next, kv_ss)
   end function

#ifdef _FHASH_FINAL_IS_IMPLEMENTED
   impure elemental subroutine clear_final(this)
      type(_FHASH_TYPE_NAME), intent(inout) :: this

      call this%clear()
   end subroutine
#endif

   impure elemental subroutine clear(this)
      class(_FHASH_TYPE_NAME), intent(inout) :: this

      this%n_keys = 0
      if (associated(this%buckets)) then
         call clear_node_and_children(this%buckets)
         deallocate(this%buckets)
      endif
   end subroutine

   impure elemental subroutine clear_node_and_children(node)
      ! Not a recursive subroutine, because (i) this is much more performant, and
      ! (ii) gfortran thinks that it cannot be both elemental and recursive.
      _FHASH_FINAL_TYPEORCLASS(node_type), intent(inout) :: node

      type(node_type), pointer :: prev, next

      if (allocated(node%kv)) deallocate(node%kv)

      next => node%next
      do
         if (.not. associated(next)) exit
         prev => next
         next => prev%next
         deallocate(prev)
      enddo
   end subroutine

   integer function key2bucket(this, key) result(bucket_id)
      class(_FHASH_TYPE_NAME), intent(in) :: this
      KEY_TYPE, intent(in) :: key

      integer :: hash
      interface default_hash
         module procedure :: default_hash__int
         module procedure :: default_hash__int_array
      end interface

#ifdef HASH_FUNC
      hash = HASH_FUNC(key)
#else
      hash = default_hash(key)
#endif
      bucket_id = modulo(hash, size(this%buckets)) + 1
   end function

   integer function default_hash__int(key) result(hash)
      integer, intent(in) :: key

      hash = key
   end function

   integer function default_hash__int_array(key) result(hash)
      integer, intent(in) :: key(:)

      real(kind(1.0d0)), parameter :: phi = (sqrt(5.0d0) + 1) / 2
      ! Do not use `nint` intrinsic, because ifort claims that  "Fortran 2018 specifies that
      ! "an elemental intrinsic function here be of type integer or character and
      !  each argument must be an initialization expr of type integer or character":
      integer, parameter :: magic_number = 0.5d0 + 2.0d0**bit_size(hash) * (1 - 1 / phi)
      integer :: i

      hash = 0
      do i = 1, size(key)
         ! This triggers an error in `gfortran` (version 9.3.0) with the `-ftrapv` option.
         ! Compiler bug?
         hash = ieor(hash, key(i) + magic_number + ishft(hash, 6) + ishft(hash, -2))
      enddo
   end function

   subroutine begin(this, fhash_target)
      class(_FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this
      type(_FHASH_TYPE_NAME), intent(in) :: fhash_target

      call assert(associated(fhash_target%buckets), "cannot start iteration when fhash is empty")

      this%bucket_id = 1
      this%buckets_ptr => fhash_target%buckets
      this%next_node => fhash_target%buckets(1)
      this%next_is_at_base = .true.
      if (.not. allocated(this%next_node%kv)) call progress_to_next_node(this)
   end subroutine

   logical function has_next(this)
      class(_FHASH_TYPE_ITERATOR_NAME), intent(in) :: this

      has_next = associated(this%next_node)
   end function

   subroutine next(this, key, value)
      class(_FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this
      KEY_TYPE, intent(out) :: key
      VALUE_TYPE, intent(out) :: value

      call assert(this%has_next(), "next: iterator has no next element")

      key = this%next_node%kv%key
      value VALUE_ASSIGNMENT this%next_node%kv%value
      call progress_to_next_node(this)
   end subroutine

   subroutine progress_to_next_node(this)
      class(_FHASH_TYPE_ITERATOR_NAME), intent(inout) :: this

      call assert(associated(this%next_node), "INTERNAL ERROR: progress_to_next_node called on null next_node")

      do
         if (associated(this%next_node%next)) then
            this%next_node => this%next_node%next
            this%next_is_at_base = .false.
            exit
         endif

         if (this%bucket_id == size(this%buckets_ptr)) then
            this%next_node => null()
            exit
         endif

         this%bucket_id = this%bucket_id + 1
         this%next_node => this%buckets_ptr(this%bucket_id)
         this%next_is_at_base = .true.
         if (allocated(this%next_node%kv)) exit
      enddo
   end subroutine

   impure elemental subroutine assert(condition, msg)
      use, intrinsic :: iso_fortran_env, only: error_unit
      logical, intent(in) :: condition
      character(*), intent(in) :: msg

      if (.not. condition) then
         write(error_unit, '(a)') msg
         error stop
      endif
   end subroutine

   integer(c_int) function _FHASH_COMPARE_AT_IDX(c_a, c_b) bind(C)
   use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer

   type(c_ptr), value :: c_a, c_b

   integer(c_int), pointer  :: f_a, f_b

   call c_f_pointer(c_a, f_a)
   call c_f_pointer(c_b, f_b)
   _FHASH_COMPARE_AT_IDX = int(global_compare_ptr(global_sorted_kv_list_ptr(f_a)%key, &
      global_sorted_kv_list_ptr(f_b)%key), kind=c_int)
end function

function sorting_perm() result(perm)
   use, intrinsic :: iso_c_binding

   integer(c_int), allocatable, target :: perm(:)

   integer(c_int) :: i, n
   type(c_funptr) :: fun
   interface
      subroutine c_qsort(array, elem_count, elem_size, compare) bind(C, name="qsort")
         ! The function pointer has the interface
         !     int(*compar)(const void *, const void *)
         use, intrinsic :: iso_c_binding
         implicit none
         type(c_ptr), value :: array
         integer(c_size_t), value :: elem_count
         integer(c_size_t), value :: elem_size
         type(c_funptr), value :: compare
      end subroutine
   end interface

   call assert(associated(global_sorted_kv_list_ptr) .and. associated(global_compare_ptr), &
      "internal error: global sorting state has not been set yet")

   n = size(global_sorted_kv_list_ptr, kind=c_int)
   allocate(perm(n))
   do i = 1, n
      perm(i) = i
   enddo
   fun = c_funloc(_FHASH_COMPARE_AT_IDX)
   if (n > 0_c_int) call c_qsort(c_loc(perm(1)), int(n, kind=c_size_t), c_sizeof(perm(1)), fun)
end function
end module

#undef _FHASH_MODULE_NAME
#undef _FHASH_TYPE_NAME
#undef _FHASH_TYPE_ITERATOR_NAME
#undef _FHASH_TYPE_KV_TYPE_NAME
#undef _FHASH_SORT_KV_NAME
#undef _FHASH_FINAL_IS_IMPLEMENTED
#undef _FHASH_FINAL_TYPEORCLASS
#undef _FHASH_COMPARE_AT_IDX
#undef _FHASH_CONCAT
#undef _FHASH_PASTE
#undef FHASH_NAME
#undef KEY_TYPE
#undef VALUE_TYPE
#undef KEYS_EQUAL_FUNC
#undef HASH_FUNC
#undef VALUE_TYPE_INIT
#undef VALUE_ASSIGNMENT
