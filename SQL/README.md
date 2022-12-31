# Usage
- First build the project by running `make`
- To start the REPL run `make repl`. In the REPL, the following may be run:
  + `CREATE` queries:
    ```
    CREATE Top_languages Place INTEGER, Name VARCHAR ;
    ```
  + `INSERT` queries:
    ```
    INSERT INTO Top_languages Place, Name VALUES  100000, "Python"  ;
    INSERT INTO Top_languages Place, Name VALUES  2, "OCaml"  ;
    INSERT INTO Top_languages Place, Name VALUES  11, "C++"  ;
    INSERT INTO Top_languages Place, Name VALUES  1, "Russian"  ;
    ```
  + `SELECT` queries:
    ```
    SELECT Name FROM Top_languages ;
    ```
  + `DELETE` queries:
    ```
    DELETE FROM Top_languages WHERE Place = 1 ;
    ```
  + `UPDATE` queries:
    ```
    UPDATE Top_languages SET Place = 1 WHERE Name = "OCaml" AND Place = 2 ;
    ```
  + `DROP` queries:
    ```
    DROP Top_languages ;
    ```
- Bulk commands from file may also be run as `make repl < commands.txt`
