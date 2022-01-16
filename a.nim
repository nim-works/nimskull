type 
    Referencia  = ref object
    Ponteiro = ptr object
    NaoRef = object
        a: int

proc fn( a : int , b : any) : int =

    let r = Referencia()
    let n = NaoRef()
    var v = NaoRef()
    
    echo repr r 
    echo repr n
    return a + 2 

const a = (2 + 2 )

echo fn(a , Referencia() );

