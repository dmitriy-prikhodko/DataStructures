namespace DataStructures.Tests.HashTable

open System
open Xunit
open Swensen.Unquote
open DataStructures

    module Find = 
       
        [<Fact>] 
        let ``should raise ArgumentException for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            raises<ArgumentException> <@ HashTable.find 0 table @>
