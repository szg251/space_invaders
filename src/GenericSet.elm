module GenericSet exposing (GenericSet, empty, insert, member, remove)


type GenericSet a
    = GenericSet (List a)


empty : GenericSet a
empty =
    GenericSet []


insert : a -> GenericSet a -> GenericSet a
insert element genSet =
    let
        list =
            unwrap genSet
    in
    if List.member element list then
        genSet

    else
        GenericSet (element :: list)


remove : a -> GenericSet a -> GenericSet a
remove element genSet =
    let
        list =
            unwrap genSet
    in
    GenericSet (List.filter ((/=) element) list)


member : a -> GenericSet a -> Bool
member element genSet =
    let
        list =
            unwrap genSet
    in
    List.member element list


unwrap : GenericSet a -> List a
unwrap genSet =
    case genSet of
        GenericSet list ->
            list
