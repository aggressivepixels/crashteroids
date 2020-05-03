module Random.Extra exposing (sequence)

import Random exposing (Generator)


sequence : List (Generator a) -> Generator (List a)
sequence =
    List.foldr (Random.map2 (::)) (Random.constant [])
