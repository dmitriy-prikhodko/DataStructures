namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module IsEmpty = 
        open System

        [<Fact>] 
        let ``should return true if the HashTable is empty`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.isEmpty table @>

        [<Fact>] 
        let ``should return false if the HashTable is not empty`` () =
            let table = HashTable.empty<int, int> |> HashTable.add 0 0

            test <@ HashTable.isEmpty table = false @>

        [<Theory>] 
        [<InlineData(0, 0)>]
        [<InlineData(1, 0)>]
        [<InlineData(Int32.MaxValue, 0)>]
        let ``should return true if the last element is deleted`` (key,value) =
            let table = HashTable.empty<int, int> |> HashTable.add key value |> HashTable.remove key
            test <@ HashTable.isEmpty table @>
