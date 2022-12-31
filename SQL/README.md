# Usage
- First build the project by running `make`
- To start the REPL run `make repl`. In the REPL, the following may be run:
  + `CREATE` queries:
    ```
    CREATE Persons PersonID INTEGER, LastName VARCHAR, FirstName VARCHAR, Address VARCHAR, City VARCHAR ;
    ```
  + `INSERT` queries:
    ```
    INSERT INTO Persons PersonID, LastName, FirstName, Address, City VALUES  9, "Big", "Flopa", "Jojo", "Moon" ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  20, "Nono", "Ilya"  ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  30, "Hoho",  "Jaba" ;
    INSERT INTO Persons PersonID, LastName, FirstName VALUES  90,  "Dota", "dva" ;
    ```
  + `SELECT` queries:
    ```
    SELECT PersonID, FirstName, LastName FROM Persons ;
    ```
  + `DELETE` queries:
    ```
    DELETE FROM Persons WHERE PersonID = 30 ;
    ```
  + `UPDATE` queries:
    ```
    UPDATE Persons SET PersonID = 99, LastName = Unknown WHERE PersonID = 30 AND LastName = "Hoho" ;
    ```
  + `DROP` queries:
    ```
    DROP Persons ;
    ```
- Bulk commands from file may also be run as `make repl < commands.txt`
