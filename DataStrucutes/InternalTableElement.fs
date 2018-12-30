namespace DataStructures

type InternalTableElement<'Key, 'Value> = 
    |TableElementEmpty
    |TableElementOne of 'Key * 'Value
    |TableElementList of ('Key * 'Value) list

module InternalTableElement = 
    let count = function 
        | TableElementEmpty -> 0
        | TableElementOne (_,_) -> 1
        | TableElementList list -> List.length list

    let keys = function
        | TableElementEmpty -> Seq.empty
        | TableElementOne (key, _) -> Seq.singleton key
        | TableElementList list -> 
        seq { for (key, _) in list do yield key }

    let values = function
        | TableElementEmpty -> Seq.empty
        | TableElementOne (_, value) -> Seq.singleton value
        | TableElementList list -> 
        seq { for (_, value) in list do yield value }

    let items = function
        | TableElementEmpty -> Seq.empty
        | TableElementOne (key, value) -> Seq.singleton (key,value)
        | TableElementList list -> 
        seq { for (key, value) in list do yield (key,value) }

    let tryFind (findKey: 'Key) (table:InternalTableElement<'Key, 'Value>) = 
        match table with
        | TableElementEmpty -> None
        | TableElementOne (key, value) when key = findKey -> Some value
        | TableElementOne (_, _) -> None
        | TableElementList list -> 
            match List.tryFind (fun(key, _) -> key = findKey) list with
            | None -> None
            | Some (_, value) -> Some value

    let add key value = function
        | TableElementEmpty -> TableElementOne (key, value)
        | TableElementOne (x,y) -> TableElementList [(x,y); (key,value)]
        | TableElementList list -> TableElementList ((key, value)::list)

    let remove key table = 
        match table with
        | TableElementEmpty -> TableElementEmpty
        | TableElementOne (x,_) when x = key-> TableElementEmpty
        | TableElementList list -> 
            match List.filter (fun (x,y) -> x <> key) list with
            |[] -> TableElementEmpty
            |head::[] -> TableElementOne head
            |tail -> TableElementList tail
        |_ -> table

