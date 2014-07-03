from HTTP4Store import HTTP4Store
from SPARQLWrapper import SPARQLWrapper, JSON
import simplejson as json
import re

class QuadstoreAdmin:
	def __init__(self):
		self.db = HTTP4Store('http://localhost:8080')
		self.query = SPARQLWrapper('http://localhost:8080/sparql/')
		
	def import4store(self, file):
		with open(file) as f:
			for line in f:
				self.insertTriples(line)
						
	def insertTriples(self, triples):
		try:
			self.db.append_graph('http://rdf.recommender', triples, 'n3')
			return 'OK'
		except:
			return 'KO'
		
	def test(self):
		try:
			return self.query.setQuery('SELECT * WHERE { ?s ?p ?o } LIMIT 10')
		except:
			print '4store is not running up'
		
	def searchID(self, id):
		self.query.setQuery("""
			DESCRIBE ?book
			WHERE {
				?book <http://rdf.recommender/id> """ + str(id) + """ .
			}
		""")
		self.query.setReturnFormat(JSON)
		return self.query.query().convert()
		
	def searchSimilar(self, criteria, baseline):
		where = ''
		for elem in criteria:
			where = where + '?uri <' + elem['value'] + '> ?' + \
				elem['name'] + ' .\n' + '?book <' + elem['value'] + '> ?' + \
				elem['name'] + ' .\n'
		query = """
			DESCRIBE ?book
			WHERE {\n""" + where
		if re.match("[0-9]+", baseline): # If what we have from the baseline book is the ID
			query = query + """?uri <http://rdf.recommender/id> """ + baseline + """ .\n}"""
		else:
			query = query + """
				FILTER (?uri = <""" + baseline + """>)\n
			}"""
		query = query + """ LIMIT 8"""
		#print query
		self.query.setQuery(query)
		self.query.setReturnFormat(JSON)
		return self.query.query().convert()
        
if __name__ == '__main__':
    client = QuadstoreAdmin()
    print(client.searchID(8176))
	#client.test()
	#books = client.searchID('1')
	#for book in books:
	#	print book['http://dbpedia.org/property/name'][0]['value']
	#book = client.searchID('10')
	#for b in book:
	#	print book[b]['http://dbpedia.org/property/name'][0]['value']
	#client.import4store('../data/rdf_recommender.rdf')
	#criteria = [
	#	{ "name":"author", "value":"http://dbpedia.org/property/author" },
	#	{ "name":"genre", "value":"http://dbpedia.org/ontology/literaryGenre" }
	#]
	#			"genre": { "genre":"http://dbpedia.org/ontology/literaryGenre" },
	#			"author_genre": { "author":"http://dbpedia.org/property/author", "genre":"http://dbpedia.org/ontology/literaryGenre"}
	#		},
	#result = client.searchSimilar(criteria, 'http://dbpedia.org/resource/Dragonfly_in_Amber')
	#result = client.searchSimilar(criteria, '1')
	
	
