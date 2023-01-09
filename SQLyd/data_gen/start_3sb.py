
import numpy as np
import pprint
import sys
import mysql.connector
from mysql.connector import errorcode
import random
import string
import time
import pyodbc
import concurrent.futures
from multiprocessing import Process
import threading
import argparse
import warnings
from functools import wraps

def ignore_warnings(f):
	@wraps(f)
	def inner(*args, **kwargs):
		with warnings.catch_warnings(record=True) as w:
			warnings.simplefilter("ignore")
			response = f(*args, **kwargs)
		return response
	return inner


user_switch=1

# python start_3sb.py -s mysql -D ssbtest -u 10 -r 80 -H 127.0.0.1 -l root -p whydoyouwant -S 30 -mu 10 -rs 70 -ru 70 -t 1

parser = argparse.ArgumentParser(description='Command Line options for building a 3sb database')
parser.add_argument('-S', '--secs',  type=int, required=True, help='Time in seconds for the test to run')
parser.add_argument('-u', '--users', type=int,  required=True, help='Number of User tables in the database')
parser.add_argument('-mu', '--max_users',  type=int, required=True, help='User Tables to read from')
parser.add_argument('-mr', '--max_rows',  type=int,  help='Maximum rows to read from per User table. Optional Reads all rows by default')
parser.add_argument('-r', '--read-percent',  type=int, required=True, help='Workload read percentage')
parser.add_argument('-s', '--sql_version', type=str,  required=True, help='mysql or mssql')
parser.add_argument('-rs', '--rows-select',  type=int, required=True, help='Rows to select per read query')
parser.add_argument('-ru', '--rows-update',  type=int, required=True, help='Rows to update per update query')
parser.add_argument('-D', '--database_name', type=str,  required=True, help='Database Name')
parser.add_argument('-H', '--host', type=str,  required=True, help='Server FQDN or IP or Instance name')
parser.add_argument('-l', '--username', type=str,  required=True, help='Server Administrative Login. For MSSQL please use SQL authentication')
parser.add_argument('-p', '--password', type=str,  required=True, help='Server Password')
parser.add_argument('-t', '--threads', type=int,  required=True, help='Number of threads to use')
parser.add_argument('-o', '--sql_driver_number', type=int,   help='MSSQL ODBC Driver verions number: ex: ODBC Driver 17 for SQL Server')
args = parser.parse_args()

secs=args.secs
users=args.users
musers=args.max_users
max_rows=args.max_rows
read_percent=args.read_percent
sql_version=args.sql_version
rows_to_read=args.rows_select
rows_to_write=args.rows_update
database_name=args.database_name
host=args.host
username=args.username
password=args.password
threads=args.threads
sql_version_number=args.sql_driver_number
mrows = 0

if sql_version == "mssql":
	if sql_version_number is None:
		print("the -o option is a required parameter when specifying mssql as the sql version")
		print('Fatel Error, exiting')
		exit()
	sql_version_number = args.sql_driver_number
	sql_driver = "ODBC Driver {} for SQL Server".format(sql_version_number)
	temp_path = ""
	sql_config = {
			'Driver': sql_driver,
			'server': host,
			'database': database_name,
			'UID': username,
			'PWD': password
		}

if sql_version == "mysql":
	sql_version_number = ""
	mysql_config = {
			'host': host,
			'database': database_name,
			'user': username,
			'password': password,
		}
if max_rows is None:
	max_rows = 0


def count_rows():
	global max_rows
	global sql_version
	row = 0

	query = "select count(*) from user_1"
	if sql_version == "mssql":
		try:
			mssql_connect = pyodbc.connect(**sql_config)
			cursor = mssql_connect.cursor()
			cursor.execute(query)
			m_rows = cursor.fetchall();
			for rows in m_rows:
				row = rows[0]
		except pyodbc.Error as e:
			error=str(e)
			print(error)
	elif sql_version == "mysql":
		try:
			mysql_connect = mysql.connector.connect(**mysql_config)
			cursor = mysql_connect.cursor(buffered=True)
			cursor.execute(query)
			m_rows = cursor.fetchall();
			for rows in m_rows:
				row = rows[0]
		except mysql.connector.Error as e:
			error = str(e)
			print(error)
	if row == 0:
		print("Database is empty. Please run setup3sb")
		exit()
	if max_rows > row:
		mrows = row
	elif max_rows == 0:
		mrows = row
	elif max_rows < row:
		mrows = max_rows
	elif max_rows == row:
		mrows = row
	elif max_rows is None:
		mrows = row

	return mrows


def mysql_select_query(musers, mrows, mysql_connect, k):
	global read_percent
	global rows_to_read
	max_users = musers
	max_rows = mrows
	query=""

	if user_switch == 0:
		if max_users > 1:
			users = np.random.randint(1,max_users)
		else:
			users = 1
	else:
		users = k

	starting_row = np.random.randint(1, max_rows)
	ending_row = int(starting_row + rows_to_read )
	if rows_to_read == 0:
		query = "select count(c1) from user_{} where custid = {}".format(users, starting_row)
		filequery = "SELECT * FROM user_{} WHERE custid = {}".format(users, starting_row)
	elif rows_to_read > 0:
		query = "select count(c1) from user_{} where custid between {} and {} order by custid".format(users, starting_row, ending_row)
		filequery = "SELECT * FROM user_{} WHERE custid >= {} AND custid <= {}".format(users, starting_row, ending_row)
	with open("select_commands.txt", 'a') as f:
		f.write(filequery)
		f.write("\n")
	mycursor = mysql_connect.cursor(buffered=True)
	try:
		mycursor.execute(query)
	except mysql.connector.Error as e:
		error = str(e)
		print(error)



@ignore_warnings
def mysql_update_query(musers,mrows,mysql_connect,k):
	global rows_to_read
	rows_to_write = rows_to_read
	max_users = musers
	max_rows = mrows
	query = ""
	length = 370
	n_codes = 1
	alpha_num = list('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')

	if user_switch == 0:
		if max_users > 1:
			users = np.random.randint(1, max_users)
		else:
			users = 1
	else:
		users = k

	starting_row = np.random.randint(1, max_rows)
	np_codes = np.random.choice(alpha_num, size = [n_codes, length])
	codes = [code.tostring() for code in np_codes]
	column_values = ' '.join(str(x) for x in codes)
	cd = column_values.replace(" ", ",")
	ce = cd.replace("\\x00", "")
	cv = ce.replace("b'", "")
	column_value = cv.split(",")
	ending_row = int(starting_row + rows_to_write)
	if rows_to_write == 0:
		q = "update {}.user_{} set c1 = '{}' where custid = {}".format(database_name, users, column_value[0], starting_row, ending_row)
		query = q.replace("''", "'")
	elif rows_to_write > 0:
		q="update {}.user_{} set c1 = '{}' where custid between {} and {}".format(database_name, users, column_value[0], starting_row, ending_row)
		query = q.replace("''", "'")
	mycursor = mysql_connect.cursor(buffered=True)
	try:
		mycursor.execute(query)
	except mysql.connector.Error as e:
		error = str(e)
		print(error)
	mysql_connect.commit()


def mssql_select_query(muser, mrows,mssql_connect,k):
	global rows_to_read
	max_users = musers
	max_rows = mrows
	query = ""

	if user_switch == 0:
		if max_users > 1:
			users = np.random.randint(1, max_users)
		else:
			users = 1
	else:
		users = k

	starting_row = np.random.randint(1, max_rows)
	ending_row = int(starting_row + rows_to_read)
	if rows_to_read == 0:
		query = "select count(c1) from user_{} where custid = {}".format(users, starting_row)
	elif rows_to_read > 0:
		query = "select count(c1) from user_{} where custid between {} and {}".format(users, starting_row, ending_row)
	mssql_cursor = mssql_connect.cursor()
	try:
		mssql_cursor.execute(query)
	except pyodbc.Error as ex:
		sqlstate = ex.args[1]
		print(sqlstate)
	mssql_connect.commit()


@ignore_warnings
def mssql_update_query(musers, mrows, mssql_connect, k):
	global rows_to_read
	global rows_to_write
	max_users = musers
	max_rows = mrows
	query = ""
	length = 370
	n_codes = 20
	alpha_num = list('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')

	if user_switch == 0:
		if max_users > 1:
			users = np.random.randint(1, max_users)
		else:
			users = 1
	else:
		users = k
	starting_row = np.random.randint(1, max_rows)
	np_codes = np.random.choice(alpha_num, size = [n_codes, length])
	codes = [code.tostring() for code in np_codes]
	column_values = ' '.join(str(x) for x in codes)
	cd = column_values.replace(" ", ",")
	ce = cd.replace("\\x00", "")
	cv = ce.replace("b'", "")
	column_value = cv.split(",")
	ending_row = int(starting_row + rows_to_write)
	if rows_to_write == 0:
		q = "update {}.dbo.user_{} set c1 = '{}' where custid = {}".format(database_name, users, column_value[0], starting_row, ending_row)
		query = q.replace("''", "'")
	elif rows_to_write > 0:
		q = "update {}.dbo.user_{} set c1 = '{}' where custid between {} and {}".format(database_name, users, column_value[0], starting_row, ending_row)
		query = q.replace("''", "'")
	mssql_cursor = mssql_connect.cursor()
	try:
		mssql_cursor.execute(query)
	except pyodbc.ProgrammingError as ex:
		sqlstate = ex.args[1]
		print(sqlstate)
		print("Fatal Error, exiting")
		exit()


def control(dbuser, u):
	k = dbuser
	z = 1
	s = 1
	mssql_connect = ""
	mysql_connect = ""
	count = (secs * 100000)
	reads = int((read_percent / 10))
	end = 0

	if sql_version == "mysql":
		try:
			mysql_connect = mysql.connector.connect(**mysql_config)
		except mysql.connector.Error as e:
			sqlstate = e.args[0]
	elif sql_version == "mssql":
		try:
			mssql_connect = pyodbc.connect(**sql_config)
		except pyodbc.Error as ex:
			sqlstate = ex.args[1]
			print(sqlstate)
			print("Fatal Error, exiting")
			exit()

	t1 = time.perf_counter()
	while z <= count:
		t2 = time.perf_counter()
		end = int((t2-t1))
		if end == secs:
			break
		if end > secs:
			break
		while s <= 10:
			#rand = np.random.random_integers(1,10);
			if s <= reads:
				if sql_version == "mysql":
					mysql_select_query(musers, mrows, mysql_connect, k)
				elif sql_version == "mssql":
					mssql_select_query(musers, mrows, mssql_connect, k)
			elif s > reads:
				if sql_version == "mysql":
					mysql_update_query(musers, mrows, mysql_connect, k)
				elif sql_version == "mssql":
					mssql_update_query(musers, mrows, mssql_connect, k)
			s += 1
		s = 1
		z += 1
	print("User: {} stopping after {} Seconds".format(u, end))
	if sql_version == "mysql":
		mysql_connect.close()
	if sql_version == "mssql":
		mssql_connect.close()


def main():
	global mrows
	processes = []
	reads = read_percent
	u = 1
	t = 1
	dbuser = 1

	mrows = count_rows()

	while t <= threads:
		while u <= users:
			p = threading.Thread(target=control, args=(dbuser, u,))
			p.start()
			processes.append(p)
			print("**********************************************************************************************")
			print("Starting process for user {} threads {}                                                      *".format(u, threads))
			print("**********************************************************************************************")
			u += 1
			if dbuser >= musers:
				dbuser = 1
			else:
				dbuser += 1
		u += 1
		t += 1
	print("**********************************************************************************************")
	print("Starting test. Run time = {} seconds with Read Percent = {}%  *".format(secs, reads))
	print("**********************************************************************************************")
	print("\n")

	time.sleep(5)
	for process in processes:
		process.join()


if __name__ == '__main__':
	main()

