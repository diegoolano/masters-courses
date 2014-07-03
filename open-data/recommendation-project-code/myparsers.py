import re

class ParserResult:
	def __init__(self, result, error):
		self.result = result
		self.error = error
		
	def isResultError(self):
		return self.error
		
	def getResult(self):
		return self.result
		
class PrefixParser:
	match_expr = '^(\ )*[a-z]+[a-z\-]*[a-z]+(\ )+:is prefix of:(\ )+<http://[a-z]+(\.[a-z]+)+(/[a-z0-9\-]+)*/>(\ )*$'
	split_expr = ' :is prefix of: '
	
	@staticmethod
	def parse(path):
		file = open(path, 'r')
		result_ok = ''
		result_error = 'Syntax error in lines:\n'
		n_line = 0
		for line in file:
			n_line = n_line + 1
			if re.match(PrefixParser.match_expr, line):
				elem = line.split(PrefixParser.split_expr)
				result_ok = result_ok + 'PREFIX ' + elem[0].strip() + ': ' + elem[1].strip() + '\n'
			else:
				result_error = result_error + '\tLine ' + str(n_line) + ' -- ' + line.strip() + '\n'
		if result_error != 'Syntax error in lines:\n':
			return ParserResult(result_error.strip(), True)
		else:
			return ParserResult(result_ok.strip(), False)
			
class ResourceParser:
	prefixed_match_expr = '^(\ )*[a-z]+[a-z\-]*[a-z]+:[a-zA-Z]+(\ )*$'
	uri_match_expr = '^(\ )*<http://[a-z]+(\.[a-z]+)+(/[a-zA-Z]+)+>(\ )*$'
	
	@staticmethod
	def parse(path):
		file = open(path, 'r')
		result_ok = '';
		result_error = 'Syntax error in lines:\n'
		n_line = 0
		for line in file:
			n_line = n_line + 1
			if not re.match(ResourceParser.prefixed_match_expr, line) and \
				not re.match(ResourceParser.uri_match_expr, line):
					result_error = result_error + '\tLine ' + str(n_line) + ' -- ' + line.strip() + '\n'
			else:
				result_ok = result_ok + line.strip() + '\n'
		if result_error != 'Syntax error in lines:\n':
			return ParserResult(result_error.strip(), True)
		else:
			return ParserResult(result_ok.strip(), False)
				