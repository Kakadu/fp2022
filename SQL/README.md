# Usage
- First build the project by running `make`
- To start the REPL run `make repl`. In the REPL, the following may be run:
  + `CREATE` queries:
    ```
    CREATE Persons PersonID INTEGER, LastName VARCHAR, FirstName VARCHAR, Address VARCHAR, City VARCHAR ;
    ```
  + `INSERT` queries:
    ```
    INSERT INTO Persons PersonID, LastName, FirstName, Address, City VALUES  9, "Dan", "Kan", "Poni", "Moni" ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  20, "Bobr", "Ilya" ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  23, "Ogromni",  "Bobr" ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  93, "Putin", "Vladimir" ;
    ```
  + `SELECT` queries:
    ```
    SELECT PersonID, FirstName, LastName FROM Persons ;
    ```
  + `DELETE` queries:
    ```
    DELETE FROM Persons WHERE PersonID = 20 ;
    ```
  + `UPDATE` queries:
    ```
    UPDATE Persons SET PersonID = 99, LastName = Unknown WHERE PersonID = 20 AND LastName = "Ilya" ;
    ```
  + `DROP` queries:
    ```
    DROP Persons ;
    ```
- Bulk commands from file may also be run as `make repl < commands.txt`
