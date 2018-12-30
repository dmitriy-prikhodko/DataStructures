namespace DataStructures

module Hash = 
    let generate source = 1

type InternalTableElement<'Key, 'Value> = 
    |TableElementEmpty
    |TableElementOne of 'Key * 'Value
    |TableElementList of ('Key * 'Value) list

module InternalTableElement = 
    let count = function 
        | TableElementEmpty -> 0
        | TableElementOne (_,_) -> 0
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

    let tryFind (findKey: 'Key) (table:InternalTableElement<'Key, 'Value>) = 
        match table with
        | TableElementEmpty -> None
        | TableElementOne (key, value) when key = findKey -> Some value
        | TableElementOne (_, _) -> None
        | TableElementList list -> 
            match List.tryFind (fun(key, _) -> key = findKey) list with
            | None -> None
            | Some (_, value) -> Some value



type InternalTable<'Key, 'Value> = 
    | TableEmpty
    | TableArray of InternalTableElement<'Key, 'Value>[]


module InternalTable = 
    let empty = TableEmpty

    let isEmpty = function 
        | TableEmpty -> true
        | _ -> false
    let count = function
        | TableEmpty -> 0
        | TableArray array -> array |> Array.fold (fun x y -> x + InternalTableElement.count y) 0

    let keys = function
        | TableEmpty -> Seq.empty
        | TableArray array -> array |> Array.fold (fun x y -> Seq.append x (InternalTableElement.keys y)) Seq.empty

    let values = function
        | TableEmpty -> Seq.empty
        | TableArray array -> array |> Array.fold (fun x y -> Seq.append x (InternalTableElement.values y)) Seq.empty

    let tryFind (key:'Key) (table:InternalTable<'Key, 'Value>)  = 
        match table with
        | TableEmpty -> None
        | TableArray array -> 
            let hash = Hash.generate key
            if hash >= Array.length array then 
                None
            else
                InternalTableElement.tryFind key array.[hash]

    let find key table = 
        match tryFind key table with
        | Some value -> value
        | None -> raise (System.ArgumentException(sprintf "The key %A is not found" key))

    let containsKey key table = 
        match tryFind key table with
        | Some _ -> true
        | None -> false

    let containsValue value table = 
        match Seq.tryFind (fun x-> x=value) (values table) with
        | Some _ -> true
        | None -> false

type HashTable<'Key, 'Value when 'Key: comparison and 'Value : equality> (initTable: InternalTable<'Key, 'Value>) = 
    let table = initTable

    member this.IsEmpty = InternalTable.isEmpty table
    member this.Count = InternalTable.count table

    member this.Keys = InternalTable.keys table
    member this.Values = InternalTable.values table

    member this.TryFind<'Key, 'Value> (key: 'Key) = InternalTable.tryFind key table
    member this.Find<'Key, 'Value> key : 'Value  = InternalTable.find key table

    member this.ContainsKey key = InternalTable.containsKey key table
    member this.ContainsValue value = InternalTable.containsValue value table


module HashTable = 
    let add (key:'Key) (value: 'Value) (table: HashTable<'Key, 'Value'>) : HashTable<'Key, 'Value'> = new HashTable<'Key, 'Value'>(InternalTable.empty)
    let remove (key: 'Key) (table: HashTable<'Key, 'Value>) : HashTable<'Key, 'Value> = new HashTable<'Key, 'Value>(InternalTable.empty)

    let find (key: 'Key) (table: HashTable<'Key, 'Value>): 'Value = table.Find key
    let tryFind (key: 'Key) (table: HashTable<'Key, 'Value>): Option<'Value> = table.TryFind key 
    let containsKey (key: 'Key) (table: HashTable<'Key, 'Value'>) : bool = table.ContainsKey key
    let containsValue (value: 'Value) (table: HashTable<'Key, 'Value>): bool = table.ContainsValue value

    let isEmpty (table: HashTable<'Key, 'Value>) : bool = table.IsEmpty
    let count (table: HashTable<'Key, 'Value>) : int = table.Count
    let keys (table: HashTable<'Key, 'Value>) : seq<'Key> = table.Keys
    let values (table: HashTable<'Key, 'Value>) : seq<'Value> = table.Values

    let empty<'Key, 'Value when 'Key: comparison and 'Value : equality> : HashTable<'Key, 'Value> = new HashTable<'Key, 'Value>(InternalTable.empty) 