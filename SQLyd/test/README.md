# Testing
## Generating data via SQL Storage Benchmark
- First generate the 1GB data (split into ten tables each of size 100MB) using
  SSB (set database name, username, password, port appropriately):
```
python setupssb.py -s mysql -d 1GB -ut 10 -D ssbtest -H 127.0.0.1 -l root -p root -P 3306
```
This will populate the local MySQL instance with ten tables, each of size 100MB.
- Then start `mysql` and export tables into csv files using the following
  command (change `user_1` for each table from `user_1` to `user_10`):
```
SELECT * from user_1
INTO OUTFILE './user_1.csv'
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n';
```
Now you will have 10 csv files generated, each of size 100MB.
- Copy the generated csv files into the `csv_files` directory in the project.
  Note that each generated table (named as `table_main.csv`) should also be
  accompanied by a file describing column headers and types (named as
  `table_title.csv`). Thus, I copied `user_1.csv` as `user1_main.csv` and added
  a describing file called `user1_title.csv`.
- Start the repl using `make repl`
- Load any table using `READ user1 ;`
## Running queries
- I have modified `start_3sb.py` to output the commands into a file named
  `select_commands.txt`. Run SSB using the following (again, set database name,
  username, password, port appropriately):
```
python start_3sb.py -s mysql -D ssbtest -u 10 -r 80 -H 127.0.0.1 -l root -p root -S 30 -mu 10 -rs 70 -ru 70 -t 1
```
```
make repl < select_commands.txt
```
