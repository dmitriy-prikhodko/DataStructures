namespace DataStructures.Tests.HashTable

open System
open Xunit
open Swensen.Unquote
open DataStructures

    module Remove = 

        [<Fact>] 
        let ``should not raise any exception if the element is not present`` () =
            let table = HashTable.empty<int, int>
            raises<ArgumentException> <@ HashTable.remove 0 table @>

        [<Theory>] 
        [<InlineData(0)>]
        [<InlineData(1)>]
        [<InlineData(Int32.MaxValue)>]
        let ``should remove element with the key from the HashTable`` (key) =
            let table = HashTable.empty<int, int> |> HashTable.add key key |> HashTable.remove key
            test <@ HashTable.containsKey key table = false@>

        [<Theory>] 
        [<InlineData(0, 3, 1, 2)>]
        [<InlineData(0, 1000, 500, 600)>]
        [<InlineData(0, 10000, 5000, 6000)>]
        let ``should remove only element with the key from the HashTable`` (startAddKey, endAddKey, startRemoveKey, endRemoveKey) =
            let mutable table = HashTable.empty<int, int>
            for key in [| startAddKey .. endAddKey |] do
                table <- HashTable.add key key table
            
            for key in [| startRemoveKey .. endRemoveKey |] do
                table <- HashTable.remove key table

            for key in Array.append [| startAddKey .. startRemoveKey - 1|] [|endRemoveKey + 1 .. endAddKey |] do
                test <@ HashTable.containsKey key table @>

            for key in [| startRemoveKey .. endRemoveKey |] do
                test <@ HashTable.containsKey key table = false @>
