import sys
import os
import mysql.connector
import numpy as np
import warnings
from functools import wraps


users = int(sys.argv[1])
user_number = int(sys.argv[2])
rows_per_user = int(sys.argv[3])
sql_version = sys.argv[4]
database_name = sys.argv[5]
host = sys.argv[6]
username = sys.argv[7]
password = sys.argv[8]
if sql_version == "mssql":
	sql_version_number = sys.argv[9]
	sql_driver = "ODBC Driver {} for SQL Server".format(sql_version_number)
if sql_version == "mysql":
	temp_path="."
	if temp_path is None:
		temp_path = ""
if sql_version == "mssql":
	sql_port = sys.argv[10]


if sql_version == "mssql":
	import pandas as pd
	from bcpandas import SqlCreds, to_sql


def ignore_warnings(f):
	@wraps(f)
	def inner(*args, **kwargs):
		with warnings.catch_warnings(record=True) as w:
			warnings.simplefilter("ignore")
			response = f(*args, **kwargs)
		return response
	return inner


if sql_version == "mssql":
	sql_config = SqlCreds(
			host,
			database_name,
			username,
			password,
			sql_version_number,
			sql_port
	)


mysql_config = {
		'host': host,
		'database': database_name,
		'user': username,
		'password': password
	}


@ignore_warnings
def build_data(filename, h, rows):
	length = 370
	n_codes = 20
	alpha_num = list('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
	complete = []
	i = h
	z = 1

	while z <= rows:
		np_codes = np.random.choice(alpha_num, size = [n_codes, length])
		codes=[code.tostring() for code in np_codes]
		column_values = ' '.join(str(x) for x in codes)
		if sql_version == "mssql":
			cd = column_values.replace(" ", ",")
			ce = cd.replace("\\x00", "")
			cv = ce.replace("b'", "")
			cb = cv.replace("'", "")
			comp = str(i) + "," + str(cb)
			cmp = comp.split(',')
			complete.append(cmp)
		if sql_version == "mysql":
			cd=column_values.replace(" ",'","')
			ce=cd.replace("\\x00","")
			cv=ce.replace("b'","")
			ci='"' + str(i) + '","' + str(cv) + '"' + "\n"
			comp=ci.replace("'","")
			complete.append(comp)
			output = open(filename, "a")
			output.writelines(complete)
			output.close()
			complete = []
		i += 1
		z += 1
	h = i
	if sql_version == "mssql":
		df = pd.DataFrame(complete, columns=['custid', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'c10', 'c11', 'c12', 'c13', 'c14', 'c15', 'c16', 'c17', 'c18', 'c19', 'c20'])
		print(df)
		table_name = 'user_' + str(user_number)
		to_sql(df, table_name, sql_config, index=False, if_exists='append')
		h = i
	return h


def mssql_load_database_tables():
	h = 1
	max_rows = 50000
	temp_path=""
	y = 0

	counter = int((rows_per_user / max_rows))
	rows_left = 0

	if counter == 0:
		filename = temp_path + "bulk_data_user_{}_{}_mssql.csv".format(user_number,rows_per_user)
		#command = "bcp.exe user_" + str(user_number) + " in " + str(temp_path) + "bulk_data_user_" + str(user_number) + "_" + str(rows_per_user) + "_mssql.csv -S " + str(host) + " -U " + str(username) +  " -P " + str(password) + " -d " + str(database_name) + " -t ""'"" -r " + '"\\n"' + " -c"
		#command = command + "\n"
		build_data(filename, h, rows_per_user)
	elif counter > 0:
		rows_left = (rows_per_user - (max_rows * counter))
		y = 1
		h = 1
		while y <= counter:
			filename = temp_path + "bulk_data_user_{}_{}_{}_mssql.csv".format(user_number, y, rows_per_user)
			#command = "bcp.exe user_" + str(user_number) + " in "  + str(temp_path) + "bulk_data_user_" + str(user_number) + "_" + str(y) + "_" + str(rows_per_user) + "_mssql.csv -S " + str(host) + " -U " + str(username) + " -P " + str(password) + " -d " + str(database_name) + " -t ""'"" -r " + '"\\n"' + " -c"
			#command = command + "\n"
			h = build_data(filename, h, max_rows)
			y += 1
	if rows_left != 0:
		filename = temp_path + "bulk_data_user_{}_{}_{}_mssql.csv".format(user_number, y, rows_left)
		#command = "bcp.exe user_" + str(user_number) + " in "  + str(temp_path) + "bulk_data_user_" + str(user_number) + "_" + str(y) + "_" + str(rows_left) + "_mssql.csv -S " + str(host) + " -U " + str(username) +  " -P " + str(password) + " -d " + str(database_name) + " -t ""'"" -r " + '"\\n"' + " -c"
		#command = command + "\n"
		build_data(filename, h, rows_left)


def mysql_load_database_tables():
	h = 1
	max_rows = 100000
	y = 0

	connect_string = {
		'host': host,
		'database': database_name,
		'user': username,
		'password': password,
		'allow_local_infile': 'True'
	}

	total_num_rows = int((rows_per_user * users))
	counter = int((rows_per_user / max_rows))
	rows_left = 0

	if counter == 0:
		filename = temp_path + "bulk_data_user_{}_{}_mysql.csv".format(user_number,rows_per_user)
		print("Writing out data file Here  {}".format(filename))
		build_data(filename, h, rows_per_user)
		load_connect=mysql.connector.connect(**connect_string)
		loader="load data local infile '" + str(temp_path) + "bulk_data_user_" + str(user_number) + "_" + str(rows_per_user) + "_mysql.csv' into table " + str(database_name) + ".user_" + str(user_number) + "  fields terminated by ',' enclosed by '\"' lines terminated by   \"\\n\" """
		load_cursor=load_connect.cursor()
		try:
			load_cursor.execute(loader)
			load_connect.commit()
		except mysql.connector.Error as e:
			error=str(e)
			print(error)
		load_connect.close()
		print("Deleteing {}".format(filename))
		os.remove(filename)
	elif counter > 0:
		rows_left = (rows_per_user - (max_rows * counter))
		y = 1
		h = 1

		while y <= counter:
			filename = temp_path + "bulk_data_user_{}_{}_{}_mysql.csv".format(user_number, y, max_rows)
			print("Writing out data file {}".format(filename))
			h = build_data(filename, h, max_rows)
			load_connect = mysql.connector.connect(**connect_string)
			loader = "load data  local infile '" + str(temp_path) + "bulk_data_user_" + str(user_number) + "_" + str(y) + "_" + str(max_rows) + "_mysql.csv' into table " + str(database_name) + ".user_" + str(user_number) + "  fields terminated by ',' enclosed by '\"' lines terminated by  '\n'"""
			load_cursor = load_connect.cursor()
			try:
				load_cursor.execute(loader)
				load_connect.commit()
			except mysql.connector.Error as e:
				error = str(e)
				print(error)
			load_connect.close()
			print("Deleteing {}".format(filename))
			os.remove(filename)
			y+=1
	if rows_left != 0:
		filename = temp_path + "bulk_data_user_{}_{}_{}_mysql.csv".format(user_number, y, rows_left)
		print("Writing out data file {}".format(filename))
		build_data(filename, h, rows_left)
		z = 1
		y += 1
		load_connect = mysql.connector.connect(**connect_string)
		loader = "load data local infile '" + str(temp_path) + "bulk_data_user_" + str(user_number) + "_" + str(y) + "_" + str(rows_left) + "_mysql.csv' into table " + str(database_name) + ".user_" + str(user_number) + "  fields terminated by ',' enclosed by '\"' lines terminated by  '\n'"""
		load_cursor = load_connect.cursor()
		try:
			load_cursor.execute(loader)
			load_connect.commit()
		except mysql.connector.Error as e:
			error = str(e.errno)
			print(error)
		load_connect.close()
		os.remove(filename)


def main():
	if sql_version == "mssql":
		mssql_load_database_tables()
	elif sql_version == "mysql":
		mysql_load_database_tables()


if __name__ == '__main__':
	main()



