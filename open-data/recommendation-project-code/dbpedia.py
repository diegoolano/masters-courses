from __future__ import division

import time, sys, re
from SPARQLWrapper import SPARQLWrapper, JSON, N3
from HTTP4Store import HTTP4Store
from quadstore_admin import *
from myparsers import *

class BookScoring:
	def __init__(self, books, scores):
		self.books = books
		self.scores = scores
		
	def getBooks(self):
		return self.books
		
	def getScores(self):
		return self.scores

class DBpedia:
	def __init__(self, metapath):
		self.sparql = SPARQLWrapper('http://dbpedia.org/sparql')
		self.prefix = self.parsePrefix(metapath + '/DBpedia_prefixes')
		self.resource = self.parseResource(metapath + '/DBpedia_resources')
		
	def survive(self, var):
		if (var.isResultError()):
			print var.getResult()
			sys.exit(1)
			
	def parsePrefix(self, path):
		prefix = PrefixParser.parse(path)
		self.survive(prefix)
		return prefix
		
	def parseResource(self, path):
		resource = ResourceParser.parse(path)
		self.survive(resource)
		return resource
			
	def nextVariable(self, var): # Simplified version. Only variables from a to z
		if var == '' or var == 'z':
			return 'a'
		return chr(ord(var) + 1)
		
	def constructTriples(self, book, id):
		elem = self.resource.getResult().split('\n')
		block = variable = construct = where = ''
		for e in elem:
			variable = self.nextVariable(variable)
			current = '<' + book + '> ' + e + ' ?' + variable + ' .'
			construct = construct + current + '\n'
			where = where + 'OPTIONAL { ' + current + ' }\n'
		try:
			self.sparql.setQuery(self.prefix.getResult() + '\nCONSTRUCT\n{\n' + construct + '}\nWHERE\n{\n' + where + '}')
			self.sparql.setReturnFormat(N3)
			triples = self.sparql.query().convert() + '\n<' + book + '> <http://rdf.recommender/id> ' + id + ' .'
			self.sparql.setQuery("""
				PREFIX dbpprop: <http://dbpedia.org/property/>
				CONSTRUCT {
					<""" + book + """> <http://rdf.recommender/authorName> ?name .
				} 
				WHERE {
					<""" + book + """> dbpprop:author ?author .
					?author dbpprop:name ?name .
				}
			""")
			self.sparql.setReturnFormat(N3)
			triples = triples + '\n' + self.sparql.query().convert()
			return triples
		except:
			return 'KO'
			
	def init(self, path):
		# Ready for populating the RDF triplestore
		quadstore = QuadstoreAdmin()
		file = open(path, 'r')
		file.readline() # Skip headers
		for line in file:
			id = (line.split('\t')[0]).strip()
			book = (line.split('\t')[2]).strip()
			triples = self.constructTriples(book, id)
			if triples == 'OK':
				print 'Error at constructing the triple for ' + book
			else:
				if quadstore.insertTriples(triples) == 'OK':
					print 'Book ' + book + ' successfully retrieved'
				else:
					print 'Oops... Something wrong happened when processing book: ' + book
			time.sleep(1)
			
	def computeScore(self, book, data):
		words_book = book.strip().lower().split(' ')
		words_data = data.strip().lower().split(' ')
		score = 0
		next_j = 0
		penalize = dict()
		for i in range(0, len(words_book)):
			if not (words_book[i] in penalize):
				penalize[ words_book[i] ] = True
			for j in range(next_j, len(words_data)):
				if i < len(words_book):
					#print words_book[i] + ' ' + words_data[j
					if re.search('.*' + words_data[j] + '.*', words_book[i]):
						#print str(len(words_data[j]) / len(words_book[i]))
						score = score + len(words_data[j]) / len(words_book[i])
						next_j = j + 1
						penalize[ words_book[i] ] = False
						i = i + 1
						j = len(words_data) # The first similarity found is the one considered
			for i in penalize:
				if penalize[i]:
					factor = 1
					if len(words_book) < len(words_data):
						factor = 2
					score = score - (1 - factor*len(i)/(len(book) - len(words_book) + 1))
					#print i + ' ' + str(len(i)) + ' ' + str(len(book) - len(words_book) + 1)
			#print book + ' ' + str(score)
		return score
		
	def composeQuery(self, attributes, regexp):
		select = where = variable = ''
		for attr in attributes:
			if attributes[attr]['main']:
				select = select + '?' + attr + '\n'
				where = where + '?uri <' + attributes[attr]['value'] + '> ?' + attr + ' .\n' + \
					'FILTER(REGEX(?' + attr + ', \"' + regexp + '\", \"i\"))\n'
			else:
				select = select + '?' + attr + '\n'
				cont = 1
				prev_variable = 'uri'
				for at in attributes[attr]['value']:
					if cont < len(attributes[attr]['value']):
						variable = self.nextVariable(variable)
					else:
						variable = attr
					where = where + '?' + prev_variable + ' <' + at + '> ?' + variable + ' .\n'
					cont = cont + 1
					prev_variable = variable
		return """SELECT DISTINCT\n?uri\n""" + select + """WHERE {\n""" + where + """}"""
			
	def searchby(self, attributes, data):
		elem = self.resource.getResult().split('\n')
		block = variable = select = where = ''
		for e in elem:
			variable = self.nextVariable(variable)
			current = '?' + variable
			select = select + current + ' '
			where = where + 'OPTIONAL { ?book ' + e + ' ' + current + ' . }\n'
		words = data.strip().lower().split(' ')
		regexp = ".*"
		for w in words:
			regexp = regexp + w + ".*"
		query = self.composeQuery(attributes, regexp)
		print query
		self.sparql.setQuery(query)
		self.sparql.setReturnFormat(JSON)
		books = self.sparql.query().convert()
		scores = dict()
		for book in books['results']['bindings']:
			title = book['title']['value']
			scores[ book['uri']['value'] ] = self.computeScore(title, data)
		results = []
		scoring = []
		for i in scores:
			max = i
			for j in scores:
				if scores[j] > scores[max]:
					max = j
			for book in books['results']['bindings']:
				if book['uri']['value'] == max:
					results.append(book)
					scoring.append(scores[max])
					scores[max] = -1000000000000000000 # Number small enough
		return BookScoring(results, scoring)

if __name__ == '__main__':	
	dbpedia = DBpedia('metadata')
	#rdf.init('/home/vherrero/checkpoint3/datasets/DBbook_Items_take2.tsv')
	attributes = { \
		"title":{"value":"http://dbpedia.org/property/name", "main":True},
		"author":{
			"value":["http://dbpedia.org/property/author", "http://dbpedia.org/property/name"],
			"main":False 
		}
	}
	data = "n var"
	result = dbpedia.searchby(attributes, data)
	books = result.getBooks()
	scores = result.getScores()
	for book in range(len(books)):
		print books[book]['title']['value'] + ' ' + str(scores[book])
	#print books
	#print dbpedia.constructTriples('http://dbpedia.org/resource/The_Gadfly', '-1')
	
	
