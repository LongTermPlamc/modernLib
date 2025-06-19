module modernLib
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, modernLib!"
  end subroutine say_hello
end module modernLib
