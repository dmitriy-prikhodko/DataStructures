namespace DataStructures

type HashTableArrayElement<'Key, 'Value> = 
    | Empty
    | One of 'Key * 'Value
    | List of ('Key * 'Value) list

module HashTableArrayElement = 

    let keys = function
        | Empty -> Seq.empty
        | One (key, _) -> Seq.singleton key
        | List list -> 
        seq { for (key, _) in list do yield key }

    let values = function
        | Empty -> Seq.empty
        | One (_, value) -> Seq.singleton value
        | List list -> 
        seq { for (_, value) in list do yield value }

    let items = function
        | Empty -> Seq.empty
        | One (key, value) -> Seq.singleton (key,value)
        | List list -> 
        seq { for (key, value) in list do yield (key,value) }

    let tryFind (findKey: 'Key) (table:HashTableArrayElement<'Key, 'Value>) = 
        match table with
        | Empty -> None
        | One (key, value) when key = findKey -> Some value
        | One (_, _) -> None
        | List list -> 
            match List.tryFind (fun(key, _) -> key = findKey) list with
            | None -> None
            | Some (_, value) -> Some value

    let add key value = function
        | Empty -> One (key, value)
        | One (x,y) -> List [(x,y); (key,value)]
        | List list -> List ((key, value)::list)

    let remove key table = 
        match table with
        | Empty -> Empty
        | One (x,_) when x = key-> Empty
        | List list -> 
            match List.filter (fun (x,y) -> x <> key) list with
            |[] -> Empty
            |head::[] -> One head
            |tail -> List tail
        |_ -> table

