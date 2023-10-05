program matrix_decrypt
  implicit none
  integer, parameter :: n = 3 ! Size of the matrix (you can change this)
  real(8), dimension(n,n) :: matrix
  character(100) :: encrypted_message
  character(100) :: decrypted_message
  real(8) :: determinant
  integer :: i, j

  ! Initialize the matrix (you should replace this with your own matrix)
  matrix = reshape([1.0, 2.0, 3.0, &
                    4.0, 5.0, 6.0, &
                    7.0, 8.0, 9.0], [n, n])

  ! Read the encrypted message
  write(*,*) "Enter the encrypted message:"
  read(*,*) encrypted_message

  ! Calculate the determinant of the matrix
  determinant = det(matrix)

  if (determinant == 0.0) then
    write(*,*) "Error: The matrix is not invertible. Decryption not possible."
  else
    ! Decrypt the message using the matrix
    decrypted_message = ""

    do i = 1, len_trim(encrypted_message)
      do j = 1, n
        decrypted_message(i:i) = decrypted_message(i:i) // &
          char(ichar(encrypted_message(i:i)) - int(matrix(i, j) / determinant))
      end do
    end do

    write(*,*) "Decrypted message:"
    write(*,*) decrypted_message
  end if

end program matrix_decrypt
