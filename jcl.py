import ply.lex as lex
import re

sym_table = []
num_exec = 0
num_dd = 0
is_job_initialised = 0


####################################################	  Programms		####################################################

prog_list = ['ssort', 'ssum', 'smean']

def ssort(d):
	d = sorted(d)
	return d

def ssum(d):
	s = sum(d)
	return s

def smean(d):
	m = sum(d)/float(len(d))
	return m


####################################################	Job according to sym_table		####################################################

#	Running job when input is done with the /* symbol
def run_job(tbl):
	#print('yeah i definitely work yeah')

	print('Symbol Table')
	for line in tbl:
		print(line[0], line[1], line[2])

	step_list = []

	for i in range(len(tbl)):
		if tbl[i][1] == 'prog_name_' + str(len(step_list)):
			step_list.append([tbl[i][2], tbl[i+1][2]])

	for i in range(len(step_list)):
		in_flag = 0
		out_flag = 0
		for j in range(len(tbl)):

			if (tbl[j][2] == str(i)) and (tbl[j][0] == 'assignment'):
				step_list[i].append([tbl[j-1][2], tbl[j+1][2]])
				if tbl[j+1][2] == 'INP':
					in_flag = 1
				elif tbl[j+1][2] == 'OUTP':
					out_flag = 1
		if in_flag == 0:
			step_list[i].append([-1, 'INP'])
		if out_flag == 0:
			step_list[i].append([-1, 'OUTP'])

	k = 0
	print('\n')
	#print(step_list)
	for s in step_list:
		run_step(s)
		print('Step ',k, ' is done')
		k += 1

import sys	

def run_step(sdata):
	if (sdata[2][1] == 'INP') and (sdata[2][0] != -1):
		try:
			with open(sdata[2][0], "r") as fd:
				d = fd.readline()
				d = d.split(' ')
				d = map(float, d)
				fd.close()
		except Exception as e:
			print ('file cannot be opened ' + sdata[2][0])

		
	elif (sdata[3][1] == 'INP') and (sdata[3][0] != -1):
		try:
			with open(sdata[3][0], "r") as fd:
				d = fd.readline()
				d = d.split(' ')
				d = map(float, d)
				fd.close()
		except Exception as e:
			print ('file cannot be opened ' + sdata[3][0])

	else:
		d = []
		for sy in sdata[1]:
			d.append(float(sy))
	
	res = -1.0
	
	if sdata[0] == 'ssort':
		res = ssort(d)
	elif sdata[0] == 'ssum':
		res = ssum(d)
	elif sdata[0] == 'smean':
		res = smean(d)
	

	if (sdata[2][1] == 'OUTP') and (sdata[2][0] != -1):
		try:
			with open(sdata[2][0], "w") as fd:
				fd.write(str(res))
				print('Result writter to ', sdata[2][0])
		except Exception as e:
			print ('file cannot be opened ' + sdata[2][0])

		
	elif (sdata[3][1] == 'OUTP') and (sdata[3][0] != -1):
		try:
			with open(sdata[2][0], "w") as fd:
				fd.write(str(res))
				print('Result writter to ', sdata[2][0])
		except Exception as e:
			print ('file cannot be opened ' + sdata[2][0])
	else:
		print(res)


####################################################		Lexer		####################################################

#	Reserved words which can't be variables names
reserved = {
	'JOB' : 'JOB',
	'USRNAME' : 'USRNAME',
	'ID' : 'ID',
	'CLASS' : 'CLASS',
	'PRTY' : 'PRTY',

	'EXEC' : 'EXEC',
	'PRG' : 'PRG',
	'PARMS' : 'PARMS',

	'DD' : 'DD',
	'DSF' : 'DSF',
	'TYPE' : 'TYPE',
	'INP' : 'INP',
	'OUTP' : 'OUTP',

	'/*' : 'END',
	'//' : 'START',
	'//*' : 'COMMENT'
}

#	Full token list
tokens =list(reserved.values()) + ['NAME', 'EQ', 'FILENAME', 'NUM', 'SMALLNUM']

#	Token templates with regular expressions
t_NAME = r'[a-zA-z]{1}\w{0,7}'
t_EQ = r'='

#	Ignored characters
t_ignore = r' '

def t_FILENAME(t): r'\w+\.\w*'; return t
def t_JOB(t): r'JOB'; return t
def t_USRNAME(t): r'USRNAME'; return t
def t_ID(t): r'ID'; return t
def t_CLASS(t): r'CLASS'; return t
def t_PRTY(t): r'PRTY'; return t

def t_EXEC(t): r'EXEC'; return t
def t_PRG(t): r'PRG'; return t
def t_PARMS(t): r'PARMS=\w*(\s\w+)*'; return t

def t_DD(t): r'DD'; return t
def t_DSF(t): r'DSF'; return t
def t_TYPE(t): r'TYPE'; return t
def t_INP(t): r'INP'; return t
def t_OUTP(t): r'OUTP'; return t

def t_END(t): r'/\*'; return t
def t_START(t): r'//\b'; return t
def t_COMMENT(t): r'//\*(\s*\w*)*'; return t

def t_NUM(t): r'\d{4}'; return t
def t_SMALLNUM(t): r'1[0-5]|[0-9]'; return t


#	Error Handler
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

#	Building Lexer
lexer = lex.lex()  

#	Testing lexer eleents
#lexer.input('END')
#tok = lexer.token()
#print(tok)

####################################################		Parser Aid			####################################################

def write_to_sym_table(data):
	global sym_table
	global num_exec, num_dd

	if data[3] == 'JOB':
		#sym_table = []
		sym_table.append(['NAME', 'job_name', data[2]])
	elif data[3] == 'EXEC':
		sym_table.append(['NAME', 'exec_name_' + str(num_exec), data[2]])
		num_exec += 1
	elif data[3] == 'DD':
		sym_table.append(['NAME', 'dd_name_' + str(num_dd), data[2]])
		num_dd += 1

#	Parsing parameter list for EXEC
def dumb_params_parser(par):
	global sym_table, prog_list
	global num_exec, num_dd

	#if par.rindex(' = '):
	#	par = par.replace(' = ', '=')
	par = par.replace('PARMS=', '')
	list_par = par.split(' ')
	#for param in list_par:
		#if re.search(r'\d+', s):
		#	param = int(param)
		#elif re.search(r'\d+(\.\d)+', s):
		#	param = float(param)
	sym_table.append(['parameters', 'params_exec_' + str(num_exec), list_par])


####################################################	YACC-based parser		####################################################

import ply.yacc as yacc

#	Production for all possible lines

#	JOB — init job with job name, username (USRNAME), id (ID), class (CLASS), priority (PRTY)
#	EXEC — init step of the job with exec name, program name (PRG), parameters (PARMS)
#	DD — init external sources
#	Comment
#	Ending statement
def p_cmdline(p):
	'''cmdline : START NAME JOB job_params
				| START NAME EXEC exec_params
				| START NAME DD dd_params
				| fin
				| cmt'''
	if p[1]:
		if p[4] != -1:
			write_to_sym_table(p)

#	Job name — 1-8 Word Characters
#	Username (USRNAME) — 1-8 Word Characters
#	id (ID) — 1-4 Digits
#	Class (CLASS) —  1-8 Word Characters
#	Priority (PRTY) - Number between 1 and 100
def p_job_params(p):
	'job_params : USRNAME EQ NAME ID EQ NUM CLASS EQ NAME PRTY EQ SMALLNUM'

	global sym_table
	global is_job_initialised

	if is_job_initialised == 0:
		is_job_initialised = 1
		sym_table.append(['NAME', 'username', p[3]])
		sym_table.append(['NUM', 'id', p[6]])
		sym_table.append(['NAME', 'job_class', p[9]])
		sym_table.append(['SMALLNUM', 'job_priority', p[12]])
		p[0] = 1
	else:
		print('Cannot initialise job twice!')
		p[0] = -1

#	Execution step name — 1-8 Word Characters
#	Program name (PRG) — 1-8 Word Characters
#	Params (PARMS) — list of parameters needed for the program
def p_exec_params(p):
	'exec_params : PRG EQ NAME PARMS'

	global sym_table
	global num_exec, num_dd
	global is_job_initialised
	
	if is_job_initialised == 1:
		if p[3] not in prog_list:
			print('Error in EXEC ' + p[3] + ': no such program exists')
			p[0] = -1
		else:
			sym_table.append(['step', 'prog_name_'+str(num_exec), p[3]])
			dumb_params_parser(p[4])
			p[0] = 1
	else:
		print('Job needs to be initialised first!')
		p[0] = -1

#	Dataset name — 1-8 Word Characters
#	Dataset File (DSF) — No particular length restrictions, word characters, must have an extension!
# 	Type (TYPE) — INP/OUTP
def p_dd_params(p):
	'''dd_params : SMALLNUM DSF EQ FILENAME TYPE EQ INP
				 | SMALLNUM DSF EQ FILENAME TYPE EQ OUTP'''

	global sym_table
	global num_exec, num_dd
	global is_job_initialised

	if is_job_initialised == 1:
		if (int(p[1]) > num_exec-1):
			print('Data must be assigned to an existing program!')
			p[0] = -1
		else:
			sym_table.append([p[2], 'file_name_'+str(num_dd), p[4]])
			sym_table.append(['assignment', 'step_num_'+str(num_dd), p[1]])
			sym_table.append(['TYPE', 'file_type_'+str(num_dd), p[7]])
	else:
		print('Job needs to be initialised first!')
		p[0] = -1	

#	Comment: starts with //*
def p_cmt(p):
	'cmt : COMMENT'

	print('')

#	Ending Statement — /*
def p_fin(p):
	'fin : END'
	global sym_table
	run_job(sym_table)

#	Syntax Error handling
def p_error(p):
    print("Syntax error in input!")

parser = yacc.yacc()
s = ''

print('Welcome!')
print('Please input your job\n')
while True:
	s = input()
	result = parser.parse(s)
	if s == '/*':
		print('Job input is successfully done!')
		break













