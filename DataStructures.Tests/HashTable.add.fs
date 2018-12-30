namespace DataStructures.Tests.HashTable

open System
open Xunit
open Swensen.Unquote
open DataStructures

    module Add = 
       
        [<Theory>]
        [<InlineData(0, 0)>]
        [<InlineData(1, 0)>]
        [<InlineData(Int32.MaxValue, 0)>]
        let ``should return HashTable with the element`` (key, value) =
            let table = HashTable.empty<int, int> |>  HashTable.add key value

            test <@ HashTable.containsKey key table = true @>

        [<Theory>] 
        [<InlineData(0, 0)>]
        [<InlineData(1, 0)>]
        [<InlineData(Int32.MaxValue, 0)>]
        let ``should raise ArgumentException in case the table contains the key`` (key, value) =
            let table = HashTable.empty<int, int> |> HashTable.add key value

            raises<ArgumentException> <@ HashTable.add key value table  @>
