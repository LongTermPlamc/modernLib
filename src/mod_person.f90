module mod_person
    type :: Person
        character(20) :: name
        integer :: age
        character(20):: occupation = "Programmer"
        character(20):: greetingMessage =""
    contains
        procedure, pass(self) :: greet
    end type Person

    interface Person
        module procedure :: personConstructor
    end interface Person

    contains
    
    pure type(Person) function personConstructor(name, age, occupation) result (res)
        character(len=*), intent(in) :: name
        integer, intent(in) :: age
        character(len=*), intent(in) :: occupation

        res%name = name
        res%age = age
        res%occupation = occupation
        if(occupation == "Pirate") then
            res%greetingMessage = "Ahoy, matey!"
        else
            res%greetingMessage = "Hi there"
        endif
    end function

    subroutine greet(self)
        class(Person), intent(in):: self
        write(*,*) "Hello, my name is "//trim(self%name)//". I work as "//trim(self%occupation)//"."
        write(*,"(A, I4, A)") "I'm ", self%age, " years old." 
    end subroutine greet
end module mod_person