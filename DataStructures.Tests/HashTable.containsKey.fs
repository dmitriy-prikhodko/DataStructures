namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module ContainsKey = 

        [<Fact>] 
        let ``should return false for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.containsKey 0 table = false @>
