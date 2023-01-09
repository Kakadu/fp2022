#!/usr/bin/env python3

import pyodbc
import mysql.connector
import sys
import time
import re
import subprocess
import datetime
from datetime import datetime
import argparse
import os
import platform


# SELECT * from user_10
# INTO OUTFILE './user_10.csv'
# FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
# LINES TERMINATED BY '\n';

# python setupssb.py -s mysql -d 1GB -ut 10 -D ssbtest -H 127.0.0.1 -l root -p whydoyouwant -P 3306
parser = argparse.ArgumentParser(description='Command Line options for building a 3sb database')
parser.add_argument('-s', '--sql-version', type=str, choices=['mysql', 'mssql'], required=True, help='mysql or mssql')
parser.add_argument('-d', '--dbsize', type=str, required=True, help='Database size in B,MB,GB,TB: example 1TB')
parser.add_argument('-ut', '--user_tables', type=int, required=True, help='Number of User tables in the database')
parser.add_argument('-D', '--database-name', type=str, required=True, help='Database Name')
parser.add_argument('-H', '--host', type=str, required=True, help='Server FQDN or IP or Instance name')
parser.add_argument('-l', '--username', type=str, required=True, help='Server Administrative Login. For MSSQL please use SQL authentication')
parser.add_argument('-p', '--password', type=str, required=True, help='Server Password')
parser.add_argument('-o', '--sql-driver-number', type=int, help='MSSQL ODBC Driver verions number: ex: ODBC Driver 17 for SQL Server')
parser.add_argument('-t', '--temp-path', type=str, help='Mysql temp path for raw data. If no path is given, path will be the 3SB working directory')
parser.add_argument('-P', '--sql-port', type=int, help='Microsoft Sql Server Dynamic Port Number')
args = parser.parse_args()

sql_version = args.sql_version
dbsize = args.dbsize
users = args.user_tables
database_name = args.database_name
host = args.host
username = args.username
password = args.password
temp_path="."

operating_system = platform.system()
abs_path = args.temp_path

if sql_version == "mssql":
	sql_version_number = args.sql_driver_number
	sql_port = args.sql_port
	sql_driver = "ODBC Driver {} for SQL Server".format(sql_version_number)

if sql_version == "mysql":
	abs_path = args.temp_path
	operating_system = platform.system()

if operating_system == "Windows" and abs_path is not None and sql_version == "mysql":
	temp_path = abs_path.replace('\\', '\\\\')
	if os.path.isdir(temp_path) is False:
		print("Path or Directory does not exist. Please enter the correct path. Exiting..")
		exit()
elif abs_path is None and sql_version == "mysql":
	get_path = os.getcwd()
	print(get_path)
	if operating_system == "Windows":
		temp_path = get_path.replace('\\', '\\\\')
		temp_path = temp_path + '\\\\'
		print(temp_path)
	print("No temporary data directory specified. The default 3SB working directory, {}, will be used instead".format(temp_path))



if sql_version == "mssql":
	sql_config = {
			'Driver': sql_driver,
			'server': host,
			'database': database_name,
			'UID': username,
			'PWD': password
	}


if sql_version == "mysql":
	mysql_config = {
			'host': host,
			'database': database_name,
			'user': username,
			'password': password
	}


def test_mssql_connection():
	print("****************************************************************************************************")
	print("Attempting to connect to the specified Microsoft Sql Server")
	print("****************************************************************************************************")
	connect_string = {
		'Driver': '{' + sql_driver + '}',
		'server': host,
		'UID': username,
		'PWD': password
	}

	try:
		sql_connection = pyodbc.connect(**connect_string)
		sql_connection.close()
		status = 0
	except pyodbc.Error as ex:
		status = 1
		sqlstate = ex.args[1]
		print(sqlstate)

	return status


def test_mysql_connection():
	print("****************************************************************************************************")
	print("Attempting to connect to the specified MySql Server")
	print("****************************************************************************************************")
	try:
		mysql_connection = mysql.connector.connect(user=username, password=password, host=host)
		status = 0
	except mysql.connector.Error as e:
		status = 1
		error = e.args[1]
		print(e.msg)

	return status


def mssql_create_database():
	connect_string = {
		'Driver': '{' + sql_driver + '}',
		'server': host,
		'UID': username,
		'PWD': password
	}

	sql_connection = pyodbc.connect(**connect_string, autocommit=True)
	sql_cursor = sql_connection.cursor()
	check_db_query = "if not exists (select name from master.dbo.sysdatabases where (name = N'{}'))" \
				   " create database {}".format(database_name,database_name)
	try:
		sql_cursor.execute(check_db_query)
	except pyodbc.Error as e:
			error = e.args[1]
			print(error)


def mssql_create_user_tables():
	number_of_users = users
	user = 1

	try:
		sql_connection = pyodbc.connect(**sql_config)
	except pyodbc.Error as e:
			sqlstate = e.args[1]
			print(sqlstate)
			exit()
	sql_cursor = sql_connection.cursor()
	print("Successfully Connected! Creating User Tables.")
	while user <= number_of_users:
		query = "CREATE TABLE [dbo].[user_{}]([custid] [int] NULL,[c1] [varchar](max) NULL,[c2] [varchar](max) NULL,[c3] [varchar](max) NULL,[c4] [varchar](max) NULL,[c5] [varchar](max) NULL,[c6] [varchar](max) NULL,[c7] [varchar](max) NULL,[c8] [varchar](max) NULL,[c9] [varchar](max) NULL,[c10] [varchar](max) NULL,[c11] [varchar](max) NULL,[c12] [varchar](max) NULL,[c13] [varchar](max) NULL,[c14] [varchar](max) NULL,[c15] [varchar](max) NULL,[c16] [varchar](max) NULL,[c17] [varchar](max) NULL,[c18] [varchar](max) NULL,[c19] [varchar](max) NULL,[c20] [varchar](max) NULL) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]".format(user)
        with open("create_commands.txt", 'w') as f:
            f.write(query)
            f.write("\n")
		try:
			sql_cursor.execute(query)
		except pyodbc.Error as e:
			sqlstate = e.args[1]
			print(sqlstate)
		user += 1
	sql_cursor.commit()
	sql_connection.close()

#replaced dbsize and users


def check_scale():
	database_size = dbsize
	number_of_users = users
	num_rows = 0
	row_size = 8192

	scale = re.findall(r'K|M|G|T',database_size,re.IGNORECASE)
	size = scale[0]

	dbi = database_size.split(size)
	database_integer = float(dbi[0])

	if size == " ":
			print("Error. Please specify size of the database using K,M,G, or T in either upper or lower case")
	elif size == "T":
		num_rows = ((database_integer * 1024 * 1024 * 1024 * 1024) / row_size)
	elif size == "G":
		print("G")
		num_rows = ((database_integer * 1024 * 1024 * 1024) / row_size)
		print(num_rows)
	elif size == "M":
		print("M")
		num_rows = ((database_integer * 1024 * 1024) / row_size)
	elif size == "K":
		print("K")
		num_rows = ((database_integer * 1024) / row_size)
	print("*****************************************************************************************************")
	print("* Database Size:" + str(database_size))
	print("* Total Number of Rows in Database:" + str(num_rows))
	rows_per_user = int((num_rows / number_of_users))
	print("* Number of Rows Per User: {}".format(rows_per_user))
	print("*****************************************************************************************************")
	return rows_per_user

#replaced users


def mssql_create_indexes():
	user = 1
	mssql_connect = pyodbc.connect(**sql_config)
	while user <= users:
		print("*****************************************************************************************************")
		print("* Creating Index for user {}                                                                        *".format(user))
		print("*****************************************************************************************************")
		query = "CREATE UNIQUE INDEX custid on {}.dbo.user_{}(custid)".format(database_name,user)
		mssql_cursor = mssql_connect.cursor()
		try:
			mssql_cursor.execute(query)
		except pyodbc.Error as e:
			sqlstate = e.args[1]
			print(sqlstate)
		mssql_cursor.commit()
		user += 1


def mysql_create_database():
	connect_string = {
		'host': host,
		'user': username,
		'password': password
	}
	mysql_connection = mysql.connector.connect(**connect_string)
	mysql_cursor = mysql_connection.cursor()
	query = "create database if not exists {}".format(database_name)
	try:
		mysql_cursor.execute(query)
	except mysql.connector.Error as e:
		error = e.args[1]


def mysql_create_user_tables():
	number_of_users = users
	u = 1

	mysql_connection = mysql.connector.connect(**mysql_config)
	mysql_cursor = mysql_connection.cursor()

	while u <= number_of_users:
		query = "create table if not exists {}.user_{} (custid INT, c1 VARCHAR(512), c2 VARCHAR(512), c3 VARCHAR(512), c4 VARCHAR(512), c5 VARCHAR(512), c6 VARCHAR(512), c7 VARCHAR(512), c8 VARCHAR(512), c9 VARCHAR(512), c10 VARCHAR(512), c11 VARCHAR(512), c12 VARCHAR(512), c13 VARCHAR(512), c14 VARCHAR(512), c15 VARCHAR(512), c16 VARCHAR(512), c17 VARCHAR(512), c18 VARCHAR(512), c19 VARCHAR(512), c20 VARCHAR(512))ENGINE=InnoDB;".format(database_name,u)
		try:
			mysql_cursor.execute(query)
		except mysql.connector.Error as e:
			error = e.args[1]
			print(error)
		u += 1
	mysql_connection.close()


def mysql_create_indexes():
	number_of_users = users
	mysql_connection = mysql.connector.connect(**mysql_config)
	mysql_cursor = mysql_connection.cursor()
	user = 1
	while user <= number_of_users:
		print("*****************************************************************************************************")
		print("*Creating Index for user {}                                                                         *".format(user))
		print("*****************************************************************************************************")
		query = "show index from user_{}".format(user)
		mysql_cursor = mysql_connection.cursor()
		mysql_cursor.execute(query)
		data = mysql_cursor.fetchone()
		if data is None:
			query = "CREATE UNIQUE INDEX custid on user_{}(custid)".format(user)
			try:
				mysql_cursor = mysql_connection.cursor()
				mysql_cursor.execute(query)
			except mysql.connector.Error as e:
				error = e.args[1]
				print(error)
		mysql_connection.commit()
		user += 1


def main():

	u = 1
	workers = 1
	w = 1
	user_number = 1
	processes = []

	rows_per_user = check_scale()

	now = datetime.now()
	t1 = time.perf_counter()
	current_time = now.strftime("%H:%M:%S")

	if sql_version == "mssql":
		status = test_mssql_connection()
		if status == 0:
			print("**********************************************************************************************")
			print("Sql Server connection was successful!")
			print("**********************************************************************************************")

			print("Creating Database")
			mssql_create_database()
			mssql_create_user_tables()
			mssql_create_indexes()
		elif status == 1:
			print("**********************************************************************************************")
			print("Fatal Error. Check that the Sql Server host you are connecting too exists and that you have the correct username and password.")
			print("**********************************************************************************************")
			sys.exit("Fatal Error: Exiting Program")
	elif sql_version == "mysql":
		status = test_mysql_connection()
		if status == 0:
			print("**********************************************************************************************")
			print("MySql Server connection was successful!")
			print("**********************************************************************************************")
			print("Creating Database")
			mysql_create_database()
			mysql_create_user_tables()
			mysql_create_indexes()
		elif status == 1:
			print("**********************************************************************************************")
			print("Fatal Error. Check that the MySql Server host you are connecting too exists and that you have the correct username and password.")
			print("**********************************************************************************************")
			sys.exit("Fatal Error: Exiting Program")

	thread_counter = users / 5
	while workers <= thread_counter:
		while u <= 5:
			print( "**********************************************************************************************")
			print("Creating Data and loading tables for user {} out of {} users.                                *".format(user_number, users))
			print("**********************************************************************************************")
			if sql_version == "mssql":
				args = "python database_loader.py {} {} {} {} {} {} {} {} {} {}".format(users,user_number,rows_per_user,sql_version,database_name,host,username,password,sql_version_number, sql_port)
			else:
				args = "python database_loader.py {} {} {} {} {} {} {} {} {}".format(users,user_number,rows_per_user,sql_version,database_name,host,username,password,temp_path)
			threads = subprocess.Popen(args,shell=True)
			processes.append(threads)
			time.sleep(.05)
			user_number += 1
			u += 1
		u = 1
		for process in processes:
			process.wait()
		workers += 1
	workers_left = users - (thread_counter * 10)
	if workers_left != 0:
		while w <= workers_left:
			print( "**********************************************************************************************")
			print("Creating Data and loading tables for user {} out of {} users.                                *".format(user_number, users))
			print("**********************************************************************************************")
			if sql_version == "mssql":
				args = "python database_loader.py {} {} {} {} {} {} {} {} {} {}".format(users,user_number,rows_per_user,sql_version,database_name,host,username,password,sql_version_number, sql_port)
			else:
				args = "python database_loader.py {} {} {} {} {} {} {} {} {}".format(users,user_number,rows_per_user,sql_version,database_name,host,username,password,temp_path)
			threads = subprocess.Popen(args,shell=True)
			processes.append(threads)
			time.sleep(.05)
			user_number += 1
			w += 1

	print("Start Time:" + current_time)
	for process in processes:
		process.wait()
	t2 = time.perf_counter()
	now = datetime.now()
	current_time = now.strftime("%H:%M:%S")
	print("Stop Time:" + current_time)
	print("Loading Database now complete")
	elapsed_time = (t2-t1)
	print("Elapsed Time: {} Seconds".format(elapsed_time))


if __name__ == "__main__":
	main()

