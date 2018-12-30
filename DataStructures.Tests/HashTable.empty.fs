namespace DataStructures.Tests

open System
open Xunit
open Swensen.Unquote
open DataStructures

module HashTable = 

    module empty = 

        [<Fact>] 
        let ``should create empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.isEmpty table @>
