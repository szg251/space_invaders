module GenericSet exposing (GenericSet, empty, insert, member, remove)


type GenericSet a
    = GenericSet (List a)


empty : GenericSet a
empty =
    GenericSet []


insert : a -> GenericSet a -> GenericSet a
insert element (GenericSet genSet) =
    if List.member element genSet then
        GenericSet genSet

    else
        GenericSet (element :: genSet)


remove : a -> GenericSet a -> GenericSet a
remove element (GenericSet genSet) =
    GenericSet (List.filter ((/=) element) genSet)


member : a -> GenericSet a -> Bool
member element (GenericSet genSet) =
    List.member element genSet
