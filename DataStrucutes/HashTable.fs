namespace DataStructures

module Hash = 
    let generate source = System.Math.Abs(source.GetHashCode())

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


type InternalTable<'Key, 'Value> = 
    | TableEmpty
    | TableArray of InternalTableElement<'Key, 'Value>[]


module InternalTable = 

    let private generateIntex key arr = (Hash.generate key) % (Array.length arr)

    let private init length = 
         Array.init length (fun _ -> TableElementEmpty) |> TableArray

    let tryFindItemWithKey key = function
        | TableEmpty -> None
        | TableArray array -> 
            let index = generateIntex key array
            InternalTableElement.tryFind key array.[index]

    let private containsItemWithKey key table = 
        match tryFindItemWithKey key table with
        | Some _ -> true
        | None -> false

    let private validateKeyForAdding key table = 
        if (containsItemWithKey key table) then 
            raise (System.ArgumentException(sprintf "The table is already contains the key %A" key))
        else
            table

    let private validateKeyForRemoving key table = 
        if (containsItemWithKey key table) then 
            table
        else
            raise (System.ArgumentException(sprintf "The table does not contain the key %A" key))

    let private initIfEmpty initialLength table = 
        match table with
        |TableEmpty -> init initialLength
        |TableArray _ -> table

    let private addItem key value table = 
        match table with
        |TableArray arr -> 
            let index = generateIntex key arr
            arr.[index] <- InternalTableElement.add key value arr.[index]
            TableArray arr
        | _ -> table

    let private removeItem key table = 
        match table with
        | TableArray arr -> 
            let index = generateIntex key arr
            arr.[index] <- InternalTableElement.remove key arr.[index]
            TableArray arr
        | TableEmpty -> table
        
    let private items = function
        | TableEmpty -> Seq.empty
        | TableArray array -> array |> Array.fold (fun x y -> Seq.append x (InternalTableElement.items y)) Seq.empty

    let private countOfItems = function
        | TableEmpty -> 0
        | TableArray array -> array |> Array.fold (fun x y -> x + InternalTableElement.count y) 0

    let private resizeIfFillFactorExceeds (fillFactor:float) table = 
        match table with
        | TableArray arr when countOfItems table >= int (fillFactor * float arr.Length) -> 
            let newArr = init (arr.Length * 2)
            for (key,value) in items table do addItem key value newArr |> ignore
            newArr
        | _ -> table

    let private markAsEmptyIfCountIsZero table =
        match table with
        | TableArray _ when countOfItems table = 0 -> TableEmpty
        | _ -> table        

    let empty = TableEmpty

    let isEmpty = function 
        | TableEmpty -> true
        | _ -> false

    let count = countOfItems

    let keys = function
        | TableEmpty -> Seq.empty
        | TableArray array -> array |> Array.fold (fun x y -> Seq.append x (InternalTableElement.keys y)) Seq.empty

    let values = function
        | TableEmpty -> Seq.empty
        | TableArray array -> array |> Array.fold (fun x y -> Seq.append x (InternalTableElement.values y)) Seq.empty

    let tryFind = tryFindItemWithKey

    let find key table = 
        match tryFind key table with
        | Some value -> value
        | None -> raise (System.ArgumentException(sprintf "The key %A is not found" key))

    let containsKey = containsItemWithKey 

    let containsValue value table = 
        match Seq.tryFind (fun x-> x=value) (values table) with
        | Some _ -> true
        | None -> false

    let add key value initialLength fillFactor = 
        validateKeyForAdding key 
        >> initIfEmpty initialLength 
        >> resizeIfFillFactorExceeds fillFactor 
        >> addItem key value

    let remove key = 
        validateKeyForRemoving key
        >> removeItem key
        >> markAsEmptyIfCountIsZero
        


type HashTable<'Key, 'Value when 'Key: comparison and 'Value : equality> (initTable: InternalTable<'Key, 'Value>) = 
    let initialLength = 1000
    let fillFactor = 0.75

    let table = initTable

    member this.Add key value = new HashTable<'Key, 'Value>(InternalTable.add key value initialLength fillFactor table)
    member this.Remove key = new HashTable<'Key, 'Value>(InternalTable.remove key table)

    member this.IsEmpty = InternalTable.isEmpty table
    member this.Count = InternalTable.count table

    member this.Keys = InternalTable.keys table
    member this.Values = InternalTable.values table

    member this.TryFind<'Key, 'Value> (key: 'Key) = InternalTable.tryFind key table
    member this.Find<'Key, 'Value> key : 'Value  = InternalTable.find key table

    member this.ContainsKey key = InternalTable.containsKey key table
    member this.ContainsValue value = InternalTable.containsValue value table


module HashTable = 
    let add (key:'Key) (value: 'Value) (table: HashTable<'Key, 'Value>) : HashTable<'Key, 'Value> = table.Add key value 
    let remove (key: 'Key) (table: HashTable<'Key, 'Value>) : HashTable<'Key, 'Value> = table.Remove key

    let find (key: 'Key) (table: HashTable<'Key, 'Value>): 'Value = table.Find key
    let tryFind (key: 'Key) (table: HashTable<'Key, 'Value>): Option<'Value> = table.TryFind key 
    let containsKey (key: 'Key) (table: HashTable<'Key, 'Value>) : bool = table.ContainsKey key
    let containsValue (value: 'Value) (table: HashTable<'Key, 'Value>): bool = table.ContainsValue value

    let isEmpty (table: HashTable<'Key, 'Value>) : bool = table.IsEmpty
    let count (table: HashTable<'Key, 'Value>) : int = table.Count
    let keys (table: HashTable<'Key, 'Value>) : seq<'Key> = table.Keys
    let values (table: HashTable<'Key, 'Value>) : seq<'Value> = table.Values

    let empty<'Key, 'Value when 'Key: comparison and 'Value : equality> : HashTable<'Key, 'Value> = new HashTable<'Key, 'Value>(InternalTable.empty)