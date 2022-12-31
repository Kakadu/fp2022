# Usage
- First build the project by running `make`
- To start the REPL run `make repl`. In the REPL, the following may be run:
  + `CREATE` queries:
    ```
    CREATE Persons PersonID INTEGER, LastName VARCHAR, FirstName VARCHAR, Address VARCHAR, City VARCHAR ;
    ```
  + `INSERT` queries:
    ```
    INSERT INTO Persons PersonID, LastName, FirstName, Address, City VALUES  9, "Du", "Chuhan", "Risley", "Moon" ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  20, "Li", "Fengyu"  ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  23, "Liu",  "Emerald" ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  93,  "Wang", "Yolanda" ;
    ```
  + `SELECT` queries:
    ```
    SELECT PersonID, FirstName, LastName FROM Persons ;
    ```
  + `DELETE` queries:
    ```
    DELETE FROM Persons WHERE PersonID = 23 ;
    ```
  + `UPDATE` queries:
    ```
    UPDATE Persons SET PersonID = 999, LastName = Unknown WHERE PersonID = 23 AND LastName = "Liu" ;
    ```
  + `DROP` queries:
    ```
    DROP Persons ;
    ```
- Bulk commands from file may also be run as `make repl < commands.txt`
