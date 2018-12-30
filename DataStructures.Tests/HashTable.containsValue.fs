namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module ContainsValue = 

        [<Fact>] 
        let ``should return false for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.containsValue 0 table = false @>
