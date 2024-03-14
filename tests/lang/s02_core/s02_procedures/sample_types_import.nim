## Example type definitions to be used in the procedure calls.

# TODO no imports in this section of tests

type
  ImRange = range[0 .. 10]
  ImArray = array[3, int]

  ImEnum = enum One, Two, Three
  ImRefOfRoot = ref object of RootObj
    a: int

  ImRefOfRef = ref object of ImRefOfRoot
    b: int

  ImObjOfRoot = object of RootObj
    a: int

  ImObjOfObj = object of ImObjOfRoot
    b: int

  ImDist = distinct string

  ImObj = object
    x: int
    y: float

  ImEnumKey = enum
    Even
    Odd

  ImEnumArray = array[ImEnumKey, string]

  ImRef = ref object
    fInt: int
    fFloat: float
    fString: string
    fEmumKey: ImEnumKey
    fObj: ImObj
    fSet: set[ImEnum]
    fRange: ImRange

    fTuple: (string, int)
    fTuple2: (ImVariant, ImVariant, ImVariant, ImVariant, ImVariant)
    fSeq: seq[string]
    fEnumArray: ImEnumArray
    fIntArray: ImArray
    fDist: ImDist

    fRefOrRoot: ImRefOfRoot
    fRefOfRef: ImRefOfRef

    fObjOfRoot: ImObjOfRoot
    fObjOfObj: ImObjOfObj

  ImVariant = object
    ignore: bool
    case kind: ImEnumKey
      of Even:
        even: int

      of Odd:
        odd: bool
        case also: uint8
          of 3:
            discard

          of 4:
            fld1, fld2, fld3: float

          else:
            `field + stropping`: string