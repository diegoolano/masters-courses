# coding=utf-8
#import urllib2
from urllib2 import Request, urlopen, URLError, HTTPError, quote
import xmltodict
import ast
import json
from mongo_admin import *
from collections import OrderedDict

class GoodReads:
    def __init__(self,query,searchby,skipset=None):
        self.base = "http://www.goodreads.com/search.xml?key=g1kKTmrhRCN8IpXI8elXQ&q="
        self.searchby = searchby
        self.initial_query = query
        if skipset == None:
            self.set_query(query)
        self.xml = ""
        self.results = ""
        self.status = ""
        self.confidence_threshold = 0.5

    def GET(self):
        req = Request(self.url)
        try:
            response = urlopen(req)
            self.xml = response.read()
            self.results = self.as_json(self.xml)
            response.close()
            self.status = 200
        except HTTPError, e:
            #print 'The server couldn\'t fulfill the request.'
            #print 'Error code: ', e.code
            self.status = e.code
            self.results = 'The server couldn\'t fulfill the request.'
        except URLError, e:
            #print 'We failed to reach a server.'
            #print 'Reason: ', e.reason
            self.status = e.reason
            self.results = 'We failed to reach a server.'

        return self.results

    def set_threshold(self,ct):
        self.confidence_threshold = float(ct)

    def set_query(self,query):
        if self.searchby == "title":
            formatted_query =  query.replace(" ","+").replace("(", "").replace(")","").replace(":", "").replace("%","").replace("&","%26")
            #formatted_query =  formatted_query.replace("ú", "%C3%BA").replace("é","%C3%A9").replace( "û", "%C3%BB").replace("é","%C3%A9").replace( "ö", "%C3%B6").replace("½","%C2%BD")
            self.query = formatted_query
            self.url = "%s%s" % ( self.base, self.query)
        else:
            #todo
            print("Error, search by: "+self.searchby+" not supported!")
            self.query = "not supported"

    def show_url(self):
        return self.url

    def get_status(self):
        return self.status

    def as_json(self,results):
        #http://stackoverflow.com/questions/191536/converting-xml-to-json-using-python
        #print(results)
        return xmltodict.parse(results)

    def get_candidate_score(self,item,qparts,qsize):
        print("in Candidate Score!")
        print(item)
        candidate = item['best_book']['title'].lower().replace(":","").replace(",","")
        #print(candidate)
        cparts = candidate.split(" ")
        csize = len(cparts)
        score = 0
        for qp in qparts:
            if qp in cparts:
                score = score + 1;

        if csize < qsize:
            score = 0
        else:
            score = (0.5 * (float(score) / qsize)) + (0.5 * (float(score) / csize) )

        return score

    def select_book_with_direct_hit_and_then_most_ratings(self,initial_query=None):
        print("in select direct hit and then most ratings")
        #for now its assumed this is being loaded from api
        if initial_query != None:
            self.initial_query = initial_query

        #print(self.results)
        max = 0
        if self.results['GoodreadsResponse']['search']['results'] == None:
            maxbook = {}
        elif self.results['GoodreadsResponse']['search']['results'] == "\n    ":
            maxbook = {}
        else:
            works = self.results['GoodreadsResponse']['search']['results']['work']
            #print(works)
            max = 0
            maxbook = {}
            for item in works:
                #print(item)
                if 'val' in item['ratings_count']:
                    item_ratings = int(item['ratings_count']['val'])
                else:
                    item_ratings = int(item['ratings_count']['#text'])

                print(item['best_book']['title']+"  --- by "+item['best_book']['author']['name']+"----+WITH RATING: "+str(item_ratings))

                if item_ratings > max and (self.initial_query.lower().strip().replace(":","").replace(",","") in item['best_book']['title'].lower().strip().replace(":","").replace(",","")):
                    max = item_ratings
                    maxbook = item

            print(maxbook)
            for k in maxbook.keys():
                if type(maxbook[k]) == OrderedDict:
                    for ck in maxbook[k].keys():
                        if ck == "#text":
                            maxbook[k]["val"] = maxbook[k][ck]
                            del(maxbook[k][ck])

            q = self.initial_query.lower().strip().replace(":","").replace(",","")
            qparts = q.split(" ")
            qsize = len(qparts)
            max = self.get_candidate_score(item,qparts,qsize)

        return {"book":maxbook,"score":max}


    def select_book_with_most_confidence(self,initial_query=None):
        print("in select with most confidence")
        #if no direct hit found, go through results
        #split query into words and check how many of those words appear in result
        #confidence_threshold = 0.5
        if initial_query != None:
            self.initial_query = initial_query

        print(self.initial_query)
        q = self.initial_query.lower().strip().replace(":","").replace(",","")
        qparts = q.split(" ")
        qsize = len(qparts)
        maxscore = 0
        if self.results['GoodreadsResponse']['search']['results'] == None:
            maxbook = {}
        elif self.results['GoodreadsResponse']['search']['results'] == "\n    ":
            maxbook = {}
        else:
            works = self.results['GoodreadsResponse']['search']['results']['work']

            maxbook = 0
            maxratings = 0
            for item in works:
                score = self.get_candidate_score(item,qparts,qsize)

                if score > maxscore:
                    maxscore = score
                    maxbook = item
                    maxratings = int(item['ratings_count']['#text'])
                elif score == maxscore and int(item['ratings_count']['#text']) > maxratings:
                    maxscore = score
                    maxbook = item
                    maxratings = int(item['ratings_count']['#text'])


        if maxscore >= self.confidence_threshold:
            print("------------found "+maxbook['best_book']['title']+" with num ratings: "+str(maxratings)+" and score: "+str(maxscore))
            print(json.dumps(maxbook))
            for k in maxbook.keys():
                if type(maxbook[k]) == OrderedDict:
                    for ck in maxbook[k].keys():
                        if ck == "#text":
                            maxbook[k]["val"] = maxbook[k][ck]
                            del(maxbook[k][ck])

            print(maxbook)
            #tm = str(maxbook).replace("#text","val")
            #print(tm)
            #tm = str(json.dumps(maxbook)).replace("#text","val")
            #formatted = ast.literal_eval(tm)
            #formatted = tm
            return {"book":maxbook,"score":maxscore}
        else:
            print("------------score below threshold of "+str(self.confidence_threshold))
            return {"book":{},"score":0}

    def select_book_with_most_ratings(self):
        #from api
        max = 0
        if self.results['GoodreadsResponse']['search']['results'] == None:
            maxbook = {}
        else:
            works = self.results['GoodreadsResponse']['search']['results']['work']
            max = 0
            maxbook = {}
            for item in works:
                if 'val' in item['ratings_count']:
                    item_ratings = int(item['ratings_count']['val'])
                else:
                    item_ratings = int(item['ratings_count']['#text'])

                if item_ratings > max:
                    max = item_ratings
                    maxbook = item

            for k in maxbook.keys():
                if type(maxbook[k]) == OrderedDict:
                    for ck in maxbook[k].keys():
                        if ck == "#text":
                            maxbook[k]["val"] = maxbook[k][ck]
                            del(maxbook[k][ck])

            q = self.initial_query.lower().strip().replace(":","").replace(",","")
            qparts = q.split(" ")
            qsize = len(qparts)
            max = self.get_candidate_score(item,qparts,qsize)

        return {"book":maxbook,"score":max}

        #tm = str(json.dumps(maxbook)).replace("#text","val")
        #formatted = ast.literal_eval(tm)
        #return formatted







    def select_book_with_direct_hit_and_then_most_ratings_from_file(self,initial_query=None):
        #for now its assumed this is being loaded from json file
        if initial_query != None:
            self.initial_query = initial_query

        #print(self.results)
        if self.results['GoodreadsResponse']['search'][0]['results'] == None:
            maxbook = {}
        elif self.results['GoodreadsResponse']['search'][0]['results'][0] == "\n    ":
            maxbook = {}
        else:
            works = self.results['GoodreadsResponse']['search'][0]['results'][0]['work']
            max = 0
            maxbook = 0
            for item in works:
                item_ratings = int(item['ratings_count'][0]['_'])
                if item_ratings > max and (self.initial_query.lower().strip().replace(":","").replace(",","") in item['best_book'][0]['title'][0].lower().strip().replace(":","").replace(",","")):
                    max = item_ratings
                    maxbook = item

         #format maxbook correctly so that you could save it into mongodb  ... START HERE TOMOORRROW
        tm = str(maxbook).replace("u'$': {u'type': u'integer'}",'u"@type" : u"integer"')
        tm = tm.replace("u'$': {u'type': u'integer', u'nil': u'true'}",'u"@type" : u"integer"')
        tm = tm.replace("u'_'","u'val'")
        tm = tm.replace("u'$': {u'type': u'Book'}",'u"@type" : u"book"')
        formatted = ast.literal_eval(tm)
        #print(formatted)
        #asjson = json.dumps(formatted)
        #print(asjson)
        #self.results = formatted
        return formatted

    def select_book_with_most_ratings_from_file(self):
        #from json file
        #print(self.results)
        if self.results['GoodreadsResponse']['search'][0]['results'] == None:
            maxbook = {}
        elif self.results['GoodreadsResponse']['search'][0]['results'][0] == "\n    ":
            maxbook = {}
        else:
            works = self.results['GoodreadsResponse']['search'][0]['results'][0]['work']
            max = 0
            maxbook = 0
            for item in works:
                item_ratings = int(item['ratings_count'][0]['_'])
                if item_ratings > max:
                    max = item_ratings
                    maxbook = item

        tm = str(maxbook).replace("u'$': {u'type': u'integer'}",'u"@type" : u"integer"')
        tm = tm.replace("u'$': {u'type': u'integer', u'nil': u'true'}",'u"@type" : u"integer"')
        tm = tm.replace("u'_'","u'val'")
        tm = tm.replace("u'$': {u'type': u'Book'}",'u"@type" : u"book"')
        formatted = ast.literal_eval(tm)
        #print(formatted)
        #asjson = json.dumps(formatted)
        #print(asjson)
        #self.results = formatted
        return formatted

    def select_book_with_most_confidence_from_file(self,initial_query=None):
        #if no direct hit found, go through results
        #split query into words and check how many of those words appear in result
        confidence_threshold = 0.5
        if initial_query != None:
            self.initial_query = initial_query

        q = self.initial_query.lower().strip().replace(":","").replace(",","")
        qparts = q.split(" ")
        qsize = len(qparts)
        if self.results['GoodreadsResponse']['search'][0]['results'] == None:
            maxbook = {}
        elif self.results['GoodreadsResponse']['search'][0]['results'][0] == "\n    ":
            maxbook = {}
        else:
            works = self.results['GoodreadsResponse']['search'][0]['results'][0]['work']
            maxscore = 0
            maxbook = 0
            maxratings = 0
            for item in works:
                candidate = item['best_book'][0]['title'][0].lower().replace(":","").replace(",","")
                cparts = candidate.split(" ")
                csize = len(cparts)
                score = 0
                for qp in qparts:
                    if qp in cparts:
                        score = score + 1;

                if csize < qsize - 1:
                    score = 0
                else:
                    score = (0.5 * (float(score) / qsize)) + (0.5 * (float(score) / csize) )

                if score > maxscore:
                    maxscore = score
                    maxbook = item
                    maxratings = int(item['ratings_count'][0]['_'])
                elif score == maxscore and int(item['ratings_count'][0]['_']) > maxratings:
                    maxscore = score
                    maxbook = item
                    maxratings = int(item['ratings_count'][0]['_'])

        print("------------found "+maxbook['best_book'][0]['title'][0]+"with num ratings: "+str(maxratings)+" and score: "+str(maxscore))
        if maxscore >= confidence_threshold:
            tm = str(maxbook).replace("u'$': {u'type': u'integer'}",'u"@type" : u"integer"')
            tm = tm.replace("u'$': {u'type': u'integer', u'nil': u'true'}",'u"@type" : u"integer"')
            tm = tm.replace("u'_'","u'val'")
            tm = tm.replace("u'$': {u'type': u'Book'}",'u"@type" : u"book"')
            formatted = ast.literal_eval(tm)
            return formatted
        else:
            print("------------score below threshold of "+str(confidence_threshold))
            return {}

    def load_json_from_file(self,number):
        conf = open("/Users/diego/htdocs/opendata/json2/"+str(number)+".json")
        self.results = json.load(conf)

    def test_query(self):
        rs = self.GET()
        print("query: "+self.query)
        print("url: "+self.url)
        if self.get_status() == 200:
            print("returned: ")
            print(json.dumps(rs))
            print("GR search: "+rs["GoodreadsResponse"]["search"]["query"])
            print("-------------------------------------")
            print("Book with Direct Hit and then ratings")
            best = self.select_book_with_direct_hit_and_then_most_ratings()
            bestj = json.dumps(best)
            if(best == 0):
                print("---none found --")
                #for b in rs["GoodreadsResponse"]["search"]["results"]["work"]:
                #   print(b["best_book"]["title"])
            else:
                print(bestj)

            print("-------------------------------------")
            print("Book with Highest Confidence")
            best = self.select_book_with_most_confidence()
            bestj = json.dumps(best)
            print(bestj)
            print(best["best_book"]["title"])

            print("-------------------------------------")
            print("Book With Most Ratings")
            best = self.select_book_with_most_ratings()
            bestj = json.dumps(best)
            print(bestj)
            print(best["best_book"]["title"])

        else:
           print(self.get_status())
           print(rs)

if __name__ == '__main__':
   #query = "DragonFly in Amber"
   query = "King & King"
   #query = "DragonFly in AmberMNX"
   #query = "Werewolf"

   query = "Pride and Prejudice"
   gr = GoodReads(query,"title")
   ma = MongoAdmin()
   #gr.test_query()
   gr.GET()
   if gr.get_status() == 200:
       best = gr.select_book_with_most_confidence()
       print(json.dumps(best))
       print(best['book'])
       mid = ma.insert(best['book'])
       #print(mid)
       ma.update_book_sqlid(mid,3812)

       #ma.delete_by_mongoid('ObjectId("539de35c653de568b8c1e7d4")')
       #ma.delete_by_sqlid(3812)


       #print(ma.find_books_by_sqlid(1))
       #ma.update_book_with_sqlid(3812,best)

   #print(best)

   #print(rsjs)


   #print(ma.test())

   #nid = ma.insert(best)
   #print(nid)
   #http://www.diveintopython.net/xml_processing/parsing_xml.html

   #OrderedDict([(u'GoodreadsResponse', OrderedDict([(u'Request', OrderedDict([(u'authentication', u'true'), (u'key', u'g1kKTmrhRCN8IpXI8elXQ'), (u'method', u'search_index')])), (u'search', OrderedDict([(u'query', u'DragonFly in Amber'), (u'results-start', u'1'), (u'results-end', u'10'), (u'total-results', u'10'), (u'source', u'Goodreads'), (u'query-time-seconds', u'0.08'), (u'results', OrderedDict([(u'work', [OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'73')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'2866304')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), (u'@nil', u'true')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), (u'@nil', u'true')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), ('val', u'1992')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'89217')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'4873')])), (u'average_rating', u'4.31'), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'5364')])), (u'title', u'Dragonfly in Amber (Outlander, #2)'), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'3617')])), (u'name', u'Diana Gabaldon')])), (u'image_url', u'http://d.gr-assets.com/books/1393788871m/5364.jpg'), (u'small_image_url', u'http://d.gr-assets.com/books/1393788871s/5364.jpg')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'39902674')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), ('val', u'15')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), ('val', u'2014')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'4')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'average_rating', u'5.00'), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'20619007')])), (u'title', u'Dragonfly in Amber'), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'7789668')])), (u'name', u'Laila Ouhssaine')])), (u'image_url', u'http://d.gr-assets.com/books/1390429220m/20619007.jpg'), (u'small_image_url', u'http://d.gr-assets.com/books/1390429220s/20619007.jpg')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'16499311')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), ('val', u'20')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), ('val', u'5')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), ('val', u'2011')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'4')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'0')])), (u'average_rating', u'5.00'), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'11558988')])), (u'title', u'Dragonfly In Amber: Summary and Study Guide'), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'4819027')])), (u'name', u'BookRags')])), (u'image_url', u'http://www.goodreads.com/assets/nocover/111x148.png'), (u'small_image_url', u'http://www.goodreads.com/assets/nocover/60x80.png')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'4')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'21798082')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), ('val', u'2012')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'1566')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'150')])), (u'average_rating', u'4.79'), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'16029144')])), (u'title', u'The Outlander Series: Outlander / Dragonfly in Amber / Voyager / Drums of Autumn / The Fiery Cross / A Breath of Snow and Ashes / An Echo in the Bone'), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'3617')])), (u'name', u'Diana Gabaldon')])), (u'image_url', u'http://d.gr-assets.com/books/1370826491m/16029144.jpg'), (u'small_image_url', u'http://d.gr-assets.com/books/1370826491s/16029144.jpg')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'25176443')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), ('val', u'17')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), ('val', u'2')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), ('val', u'2013')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'average_rating', u'2.00'), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'17959364')])), (u'title', u"100 Things You Don't Wanna Know about Dragonfly in Amber"), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'2936838')])), (u'name', u'Alice Young')])), (u'image_url', u'http://d.gr-assets.com/books/1370226336m/17959364.jpg'), (u'small_image_url', u'http://d.gr-assets.com/books/1370226336s/17959364.jpg')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'25176528')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), ('val', u'25')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), ('val', u'3')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), ('val', u'2013')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'0')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'0')])), (u'average_rating', OrderedDict([(u'@type', u'float'), ('val', u'0.0')])), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'17959449')])), (u'title', u'100 of the Most Outrageous Comments about Dragonfly in Amber'), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'5849472')])), (u'name', u'Alice Root')])), (u'image_url', u'http://d.gr-assets.com/books/1369152353m/17959449.jpg'), (u'small_image_url', u'http://d.gr-assets.com/books/1369152353s/17959449.jpg')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'2')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'40706497')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), (u'@nil', u'true')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), (u'@nil', u'true')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), (u'@nil', u'true')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'15')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'average_rating', u'4.87'), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'21406782')])), (u'title', u'Diana Gabaldon Outlander Series 7-volume Paperback Set: Outlander / Dragonfly in Amber / Voyager / Drums of Autumn / the Fiery Cross / a Breath of Snow and Ashes / an Echo in the Bone [Paperback] (Diana Gabaldon Outlander) (Diana Gabaldon Outlander)'), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'3617')])), (u'name', u'Diana Gabaldon')])), (u'image_url', u'http://www.goodreads.com/assets/nocover/111x148.png'), (u'small_image_url', u'http://www.goodreads.com/assets/nocover/60x80.png')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'41352992')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), (u'@nil', u'true')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), (u'@nil', u'true')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), (u'@nil', u'true')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'0')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'0')])), (u'average_rating', OrderedDict([(u'@type', u'float'), ('val', u'0.0')])), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'22032425')])), (u'title', u'Outlander Complete Paperback Set 1-7 (Outlander, #1 Outlander; #2 Dragonfly In Amber; #3 Voyager; #4 Drums in Autumn; #5 The Fiery Cross; #6 A Breath of Snow and Ashes; #7 An Echo in the Bone)'), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'3617')])), (u'name', u'Diana Gabaldon')])), (u'image_url', u'http://www.goodreads.com/assets/nocover/111x148.png'), (u'small_image_url', u'http://www.goodreads.com/assets/nocover/60x80.png')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'3')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'23861244')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), ('val', u'2002')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'108')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'15')])), (u'average_rating', u'4.26'), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'8624062')])), (u'title', u'Il ritorno (La saga di Claire Randall,  #3)'), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'3617')])), (u'name', u'Diana Gabaldon')])), (u'image_url', u'http://www.goodreads.com/assets/nocover/111x148.png'), (u'small_image_url', u'http://www.goodreads.com/assets/nocover/60x80.png')]))]), OrderedDict([(u'books_count', OrderedDict([(u'@type', u'integer'), ('val', u'5')])), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'23861233')])), (u'original_publication_day', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'original_publication_month', OrderedDict([(u'@type', u'integer'), ('val', u'1')])), (u'original_publication_year', OrderedDict([(u'@type', u'integer'), ('val', u'1996')])), (u'ratings_count', OrderedDict([(u'@type', u'integer'), ('val', u'124')])), (u'text_reviews_count', OrderedDict([(u'@type', u'integer'), ('val', u'16')])), (u'average_rating', u'4.20'), (u'best_book', OrderedDict([(u'@type', u'Book'), (u'id', OrderedDict([(u'@type', u'integer'), ('val', u'8624421')])), (u'title', u"L'amuleto d'ambra (La saga di Claire Randall,  #2)"), (u'author', OrderedDict([(u'id', OrderedDict([(u'@type', u'integer'), ('val', u'3617')])), (u'name', u'Diana Gabaldon')])), (u'image_url', u'http://www.goodreads.com/assets/nocover/111x148.png'), (u'small_image_url', u'http://www.goodreads.com/assets/nocover/60x80.png')]))])])]))]))]))])




   #x = {'best_book': {'author': {'id': {'#text': '3617', '@type': 'integer'}, 'name': 'Diana Gabaldon'}, 'title': 'Dragonfly in Amber (Outlander, #2)', 'id': {'#text': '5364', '@type': 'integer'}, 'image_url': 'http://d.gr-assets.com/books/1393788871m/5364.jpg', 'small_image_url': 'http://d.gr-assets.com/books/1393788871s/5364.jpg', '@type': 'Book'}, 'books_count': {'#text': '24', '@type': 'integer'}, 'original_publication_month': {'@nil': 'true', '@type': 'integer'}, 'ratings_count': {'#text': '89161', '@type': 'integer'}, 'original_publication_day': {'@nil': 'true', '@type': 'integer'}, 'original_publication_year': {'#text': '1992', '@type': 'integer'}, 'id': {'#text': '2866304', '@type': 'integer'}, 'average_rating': '4.31', 'text_reviews_count': {'#text': '4870', '@type': 'integer'}}
   #print(str(x).replace("#text","val"))



    #in python
    #{"books_count": {"@type": "integer", "#text": "9"}, "id": {"@type": "integer", "#text": "2204975"}, "original_publication_day": {"@type": "integer", "@nil": "true"}, "original_publication_month": {"@type": "integer", "#text": "10"}, "original_publication_year": {"@type": "integer", "#text": "1983"}, "ratings_count": {"@type": "integer", "#text": "1112"}, "text_reviews_count": {"@type": "integer", "#text": "21"}, "average_rating": "4.00", "best_book": {"@type": "Book", "id": {"@type": "integer", "#text": "536800"}, "title": "Unicorn Variations", "author": {"id": {"@type": "integer", "#text": "3619"}, "name": "Roger Zelazny"}, "image_url": "http://d.gr-assets.com/books/1304286167m/536800.jpg", "small_image_url": "http://d.gr-assets.com/books/1304286167s/536800.jpg"}},
    #{'best_book': {'author': {'id': {'#text': '3617', '@type': 'integer'}, 'name': 'Diana Gabaldon'}, 'title': 'Dragonfly in Amber (Outlander, #2)', 'id': {'#text': '5364', '@type': 'integer'}, 'image_url': 'http://d.gr-assets.com/books/1393788871m/5364.jpg', 'small_image_url': 'http://d.gr-assets.com/books/1393788871s/5364.jpg', '@type': 'Book'}, 'books_count': {'#text': '24', '@type': 'integer'}, 'original_publication_month': {'@nil': 'true', '@type': 'integer'}, 'ratings_count': {'#text': '89161', '@type': 'integer'}, 'original_publication_day': {'@nil': 'true', '@type': 'integer'}, 'original_publication_year': {'#text': '1992', '@type': 'integer'}, 'id': {'#text': '2866304', '@type': 'integer'}, 'average_rating': '4.31', 'text_reviews_count': {'#text': '4870', '@type': 'integer'}}


    #in mongodb as imported in from nodejs
    #{u'books_count': [{u'dollar': {u'type': u'integer'}, u'_': u'9'}], u'original_publication_month': [{u'dollar': {u'type': u'integer'}, u'_': u'10'}], u'text_reviews_count': [{u'dollar': {u'type': u'integer'}, u'_': u'21'}], u'original_publication_year': [{u'dollar': {u'type': u'integer'}, u'_': u'1983'}], u'original_publication_day': [{u'dollar': {u'type': u'integer', u'nil': u'true'}}], u'best_book': [{u'author': [{u'id': [{u'dollar': {u'type': u'integer'}, u'_': u'3619'}], u'name': [u'Roger Zelazny']}], u'title': [u'Unicorn Variations'], u'dollar': {u'type': u'Book'}, u'image_url': [u'http://d202m5krfqbpi5.cloudfront.net/books/1304286167m/536800.jpg'], u'small_image_url': [u'http://d202m5krfqbpi5.cloudfront.net/books/1304286167s/536800.jpg'], u'id': [{u'dollar': {u'type': u'integer'}, u'_': u'536800'}]}], u'ratings_count': [{u'dollar': {u'type': u'integer'}, u'_': u'1106'}], u'_id': ObjectId('534e23cd01111fe1520150b3'), u'id': [{u'dollar': {u'type': u'integer'}, u'_': u'2204975'}], u'average_rating': [u'4.00']}

