namespace DataStructures.Tests.HashTable

open Xunit
open Swensen.Unquote
open DataStructures

    module Count = 

        [<Fact>] 
        let ``should return 0 for empty HashTable`` () =
            let table = HashTable.empty<int, int>

            test <@ HashTable.count table = 0 @>

        [<Theory>] 
        [<InlineData(1)>]
        [<InlineData(2)>]
        [<InlineData(123)>]
        [<InlineData(2000)>]
        [<InlineData(999)>]
        let ``should return number of elements in the HashTable`` (n) =
            let mutable table = HashTable.empty<int, int>
            for i = 1 to n do
                table <- HashTable.add i i table
            
            test <@ HashTable.count table = n @>
