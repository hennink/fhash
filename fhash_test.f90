module tests_mod
   use ints_module
   use ints_double_mod
   use, intrinsic :: iso_fortran_env
   implicit none

contains
   subroutine test_empty_hashes()
      use i2char_mod
      use ints_double_mod
      use int_ints_ptr_mod

      type(i2char_t) :: i2char
      type(i2char_kv_t) :: i2char_kv(0)
      type(ints_double_t) :: i2r
      type(ints_double_kv_t) :: i2r_kv(0)
      type(int_ints_ptr_t) :: i2i_ptr
      type(int_ints_ptr_kv_t) :: i2i_ptr_kv(0)

      call assert(i2char%n_collisions() == 0, "expected i2char%n_collisions() == 0")
      call assert(i2char%key_count() == 0, "expected i2char%key_count() == 0")
      call i2char%reserve(1)
      call i2char%as_list(i2char_kv)
      call assert(size(i2char_kv) == 0, "expected empty list")
      
      call assert(i2r%n_collisions() == 0, "expected i2r%n_collisions() == 0")
      call assert(i2r%key_count() == 0, "expected i2r%key_count() == 0")
      call i2r%reserve(1)
      call i2r%as_list(i2r_kv)
      call assert(size(i2r_kv) == 0, "expected empty list")
      
      call assert(i2i_ptr%n_collisions() == 0, "expected i2i_ptr%n_collisions() == 0")
      call assert(i2i_ptr%key_count() == 0, "expected i2i_ptr%key_count() == 0")
      call i2i_ptr%reserve(1)
      call i2i_ptr%as_list(i2i_ptr_kv)
      call assert(size(i2i_ptr_kv) == 0, "expected empty list")
   end subroutine

   subroutine test_resize()
      use i2char_mod

      type(i2char_t) :: h
      type(i2char_kv_t) :: all_kv(200)
      integer :: i

      call h%resize(10)
      do i = 1, size(all_kv)
         all_kv(i)%key = i
         write(all_kv(i)%value, "(i0)") i
         call h%set(all_kv(i)%key, all_kv(i)%value)
      enddo
      call check_kv()

      call h%resize(100)
      call assert(h%bucket_count() >= 100, "resizing did not change bucket_count as expected")
      call check_kv()

      call h%resize(1000)
      call assert(h%bucket_count() >= 1000, "resizing did not change bucket_count as expected")
      call check_kv()

      call h%resize(1)
      call assert(h%bucket_count() <= 5, "resizing did not change bucket_count as expected")
      call check_kv()

   contains
      subroutine check_kv()
         type(i2char_kv_t) :: h_list(size(all_kv))

         call h%as_sorted_list(h_list, compare_ints)
         call assert(h_list%key == all_kv%key, "test_resize: got a bad list of keys")
         call assert(h_list%value == all_kv%value, "test_resize: got a bad list of values")
      end subroutine
   end subroutine

   subroutine test_insert_get_and_remove_int_ints_ptr()
      use int_ints_ptr_mod

      type(int_ints_ptr_t) :: h
      integer, parameter ::  num_values = 50
      type(ints_type), pointer :: pValue
      type(ints_type), target, allocatable :: pValues(:)
      logical :: success
      integer ::  i, key
      type(int_ints_ptr_iter_t) :: it

      ! prepare
      allocate(pValues(num_values))

      ! create
      call h%reserve(5)

      ! add
      do i = 1, num_values
         allocate(pValues(i)%ints(2))
         pValues(i)%ints(1) = i
         pValue => pValues(i)
         call h%set(i, pValue)
      end do

      if (h%key_count() .ne. num_values) error stop 'expect different key count'

      ! get
      do i = num_values, i, -1
         call h%get(i, pValue, success)
         if (.not. success) error stop 'expect a value for given key '
         if (pValue%ints(1) .ne. pValues(i)%ints(1)) error stop 'expect different value for given key'
      end do

      ! remove first item
      do i = 1, num_values
         if (mod(i, 5) .eq. 1) then
            call h%remove(i, success)
            if (.not. success) error stop 'expect to successfully remove item with given key '
         endif
      end do
      if (h%key_count() .ne. num_values-10) error stop 'expect different key count'

      ! remove first item (fail)
      do i = 1, num_values
         if (mod(i, 5) .eq. 1) then
            call h%remove(i, success)
            if (success) error stop 'expect that remove item with given key fails'
         endif
      end do
      if (h%key_count() .ne. num_values-10) error stop 'expect  different key count'

      ! remove middle item
      do i = 1, num_values
         if (mod(i, 5) .eq. 4) then
            call h%remove(i, success)
            if (.not. success) error stop 'expect to successfully remove item with given key '
         endif
      end do
      if (h%key_count() .ne. num_values-20) error stop 'expect different key count'

      nullify (pValue)

      ! Setup iterator.
      call it%begin(h)
      do while (it%has_next())
         call it%next(key, pValue)
         if (key .ne. pValue%ints(1)) error stop 'expect to retrieve matching key value pair'
         if (mod(key, 5) .eq. 1) error stop 'expect not to get deleted keys'
         if (mod(key, 5) .eq. 4) error stop 'expect not to get deleted keys'
      end do
      call assert(.not. it%has_next(), "inconsistent results from it%has_next()")

      call h%clear()
   end subroutine

   subroutine test_iterate()
      use ints_double_mod

      type(ints_double_t) :: h
      type(ints_double_iter_t) :: it
      type(ints_type) :: key
      real(real64) :: value
      integer :: i, j
      logical, allocatable :: found(:)
      integer :: i_found

      call h%reserve(10)
      allocate(key%ints(10))

      ! Setup keys and values.
      key%ints = 0
      do i = 1, 10
         key%ints(i) = i
         call h%set(key, i * 0.5_real64)
      enddo

      ! Setup iterator.
      call it%begin(h)

      allocate(found(10))
      found(:) = .false.

      do i = 1, 10
         call assert(it%has_next(), "it ran out of elements")
         call it%next(key, value)

         ! Check for consistency.
         i_found = nint(value / 0.5)
         if (found(i_found)) error stop 'expect no duplicate'
         found(i_found) = .true.
         do j = 1, i_found
            if (key%ints(j) /= j) error stop 'expect to get j'
         enddo
         do j = i_found + 1, 10
            if (key%ints(j) /= 0) error stop 'expect to get 0'
         enddo
      enddo
      call assert(.not. it%has_next(), 'expect to return -1')

      call h%clear()
   end subroutine

   subroutine test_as_list
      use i2char_mod

      type(i2char_t) :: h
      character(10) :: val
      integer :: i
      integer, parameter :: n_uniq = 4
      type(i2char_kv_t) :: kv_list(n_uniq)
      logical :: success

      call h%reserve(3)
      call h%set(1, "one (typo)")
      call h%set(1, "one       ")
      call h%set(0, "zero      ")
      call h%set(4, "four      ")
      call h%set(7, "seven     ")
      call assert(h%get_ptr(1) == "one", 'expected  h%get_ptr(1) == "one"')

      call h%as_list(kv_list)
      call assert(size(kv_list) == n_uniq, "kv_list has bad size")
      do i = 1, n_uniq
         call h%get(kv_list(i)%key, val, success)
         call assert(success, "key in list was not in hash")
         call assert(val == kv_list(i)%value, "bad value in list")
      enddo

      call h%as_sorted_list(kv_list, compare_ints)
      call assert(size(kv_list) == n_uniq, "sorted kv_list has bad size")
      do i = 1, n_uniq
         call h%get(kv_list(i)%key, val, success)
         call assert(success, "key in sorted list was not in hash")
         call assert(val == kv_list(i)%value, "bad value in sorted list")
      enddo
      call assert(kv_list(2:)%key - kv_list(:size(kv_list)-1)%key > 0, "sorted list should be strictly increasing")
   end subroutine

   subroutine test_large_sort()
      ! Test with an array that's too big for the stack.
      use i2char_mod

      real, parameter :: gigabytes = 0.001 ! make larger for expensive test
      type(i2char_kv_t), allocatable :: kv_list(:)
      integer, parameter :: max = 1000
      integer :: i, n, val
      real :: x

      n = nint(gigabytes * 1024**3 / (storage_size(kv_list) / 8))

      ! This list contains duplicate keys, which is not possible for lists
      ! obtained from a hash, but it should work anyway:
      allocate(kv_list(n))
      do i = 1, n
         call random_number(x)
         val = nint(x * max)
         kv_list(i)%key = val
         write(kv_list(i)%value, "(i0)") val
      enddo

      call sort_i2char(kv_list, compare_ints)

      do i = 2, n
         call assert(kv_list(i-1)%key <= kv_list(i)%key, "large sort: list should be increasing")
      enddo
      do i = 2, n
         read(kv_list(i)%value, *) val
         call assert(val == kv_list(i)%key, "large sort: bad value")
      enddo
   end subroutine

   subroutine test_get_ptr()
      use i2char_mod

      type(i2char_t) :: h
      character(:), pointer :: c
      type(i2char_kv_t), allocatable :: kv_list(:)
      integer :: i

      call h%reserve(1)

      call h%set(7, "seven     ")

      c => h%get_ptr(0)
      call assert(.not. associated(c), "expected .not. associated(c)")
      c => h%get_ptr(1)
      call assert(.not. associated(c), "expected .not. associated(c)")
      c => h%get_ptr(7)
      call assert(associated(c), "expected associated(c)")
      call assert(c == "seven", "exptected c == 'seven'")

      c(:) = 'new seven'
      c => h%get_ptr(7)
      call assert(associated(c), "expected associated(c)")
      call assert(c == 'new seven', "expected c == 'new seven'")

      do i = 1, 3
         c => h%get_ptr(2, autoval='auto two  ')
         call assert(associated(c), "expected associated(c)")
         call assert(c == 'auto two', "expected c == 'auto two'")
         call assert(h%key_count() == 2, 'expected two keys in h')
      enddo

      allocate(kv_list(h%key_count()))
      call h%as_sorted_list(kv_list, compare_ints)
      call assert(size(kv_list) == 2, "expected size(kv_list) == 2")
      call assert(kv_list%key == [2, 7], "keys should be [2, 7]")
      call assert(kv_list%value == ['auto two ', 'new seven'], "test_get_ptr: bad values")
   end subroutine

   integer function compare_ints(a, b)
      integer, intent(in) :: a, b

      compare_ints = a - b
   end function

   subroutine test_deep_storage_size()
      type(ints_double_t) :: h
      type(ints_type) :: key

      integer :: i
      integer :: s

      s = h%deep_storage_size(0123)

      call h%reserve(10)
      allocate(key%ints(2))

      do i = 1, 3
         key%ints = i
         call h%set(key, real(i, kind=real64))
      enddo
      s = h%deep_storage_size(0123)

      do i = 1, 20
         key%ints = i
         call h%set(key, real(i, kind=real64))
      enddo
      s = h%deep_storage_size(0123)
   end subroutine

   subroutine test_assignment()
      type(ints_double_t) :: a, b, c
      type(ints_type) :: keys(100)
      real(real64) :: values(size(keys))

      integer :: i

      do i = 1, size(keys)
         allocate(keys(i)%ints(3))
         keys(i)%ints = i
         values(i) = i
      enddo

      call a%reserve(10)
      do i = 1, size(keys)
         call a%set(keys(i), values(i))
      enddo
      call check_kv(a)

      c = a
      call check_kv(a)
      call check_kv(c)

      call b%reserve(1)
      b = a
      call check_kv(a)
      call check_kv(b)
      call a%clear()
      call check_kv(b)

      a = b
      call check_kv(a)
      call check_kv(b)
      call a%clear()
      call check_kv(b)
   contains
      subroutine check_kv(fhash)
         type(ints_double_t), intent(in) :: fhash

         type(ints_double_iter_t) :: iter
         type(ints_type) :: key
         real(real64) :: val
         integer :: i
         logical :: have_seen(size(keys))

         have_seen = .false.
         call iter%begin(fhash)
         do
            if (.not. iter%has_next()) exit

            call iter%next(key, val)

            i = nint(val)
            call assert(abs(val - i) <= 10*epsilon(val), "check_kv: bad value")
            call assert(key%ints == i, "check_kv: bad key")
            call assert(.not. have_seen(i), "check_kv: found the same key twice")
            have_seen(i) = .true.
         enddo
         call assert(all(have_seen), "check_kv: did not get all keys from the iterator")
      end subroutine
   end subroutine

   impure elemental subroutine assert(condition, msg)
      use, intrinsic :: iso_fortran_env, only: error_unit
      logical, intent(in) :: condition
      character(*), intent(in) :: msg

      if (.not. condition) then
         write(error_unit, '(a)') "FAILED A TEST: " // msg
         error stop
      endif
   end subroutine
end module

program fhash_test

   use, intrinsic :: iso_fortran_env
   use ints_double_mod
   use int_ints_ptr_mod
   use ints_module
   use tests_mod
   implicit none

   call test_empty_hashes()
   call test_resize()
   call test_get_ptr()
   call test_contructor()
   call test_reserve()
   call test_insert_and_get_ints_double()
   call test_insert_and_get_int_ints_ptr()
   call test_insert_get_and_remove_int_ints_ptr()
   call test_iterate()
   call test_as_list()
   call test_large_sort()
   call test_deep_storage_size()
   call test_assignment()

contains

   subroutine test_contructor()
      type(ints_double_t) h
      if (h%key_count() /= 0) error stop 'expect no keys'
   end subroutine

   subroutine test_reserve()
      type(ints_double_t) :: h

      call h%reserve(3)
      call assert(h%bucket_count() == 5, 'expected to reserve 5 buckets')
   end subroutine

   subroutine test_insert_and_get_ints_double()
      type(ints_double_t) :: h
      type(ints_type) :: key
      real(real64) :: value
      real(real64), pointer :: val_ptr
      integer :: i
      logical :: success

      call h%reserve(5)
      h%auto_resize = .false.
      allocate(key%ints(10))

      key%ints = 0
      do i = 1, 10
         key%ints(i) = i

         call h%get(key, value, success)
         if (success) error stop 'expect not found'

         val_ptr => h%get_ptr(key)
         call assert(.not. associated(val_ptr), "expected a null pointer")

         call h%set(key, i * 0.5_real64)
         call h%get(key, value)
         if (abs(value - i * 0.5_real64) > epsilon(value)) error stop 'expect to get 0.5 i'

         val_ptr => h%get_ptr(key)
         call assert(associated(val_ptr), "expected a, associated pointer")
         call assert(abs(val_ptr - i * 0.5_real64) <= epsilon(val_ptr), 'expect to get pointer value of 0.5 i')
      enddo
      if (h%key_count() /= 10) error stop 'expect key count to be 10'
      if (h%n_collisions() >= 10 .or. h%n_collisions() < 5) error stop 'expect n_collisions in [5, 10)'

      call h%clear()
      if (h%key_count() /= 0) error stop 'expect no keys'
      if (h%bucket_count() /= 0) error stop 'expect no buckets'
   end subroutine

   subroutine test_insert_and_get_int_ints_ptr()
      type(int_ints_ptr_t) :: h
      type(ints_type), target :: value
      type(ints_type), pointer :: value_ptr, value_ptr2, value_ptr3
      logical :: success
      call h%reserve(5)
      allocate(value%ints(10))
      value%ints = 0
      value_ptr => value
      call h%set(0, value_ptr)
      call h%get(0, value_ptr2, success)
      if (value_ptr2%ints(1) /= 0) error stop 'expect ints(1) to be 0'
      value_ptr2%ints(1) = 1

      call h%get(0, value_ptr3, success)
      if (value_ptr3%ints(1) /= 1) error stop 'expect ints(1) to be 1'
   end subroutine
end program
