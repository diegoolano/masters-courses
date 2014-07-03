import pymongo
import json

class MongoAdmin:
    def __init__(self):
        self.client = pymongo.MongoClient('localhost', 27017)
        self.db = self.client['recdb']
        #self.db = self.client['recommenderdb']   #for testing against old one

    def test(self):
        return self.db.books.find_one()

    def insert(self,bookjson):
        mid = self.db.books.insert(bookjson)
        return mid

    def searchby(self,request):
        fieldname = request.keys()[0]
        if fieldname == "title":
            return self.find_book_by_title(request[fieldname])
        elif fieldname == "author":
            return self.find_books_by_author(request[fieldname])
        elif fieldname == "book_sqlid":
            return self.db.books.find_one({'sqlid':request[fieldname]})
        else:
            return {}

    def find_book_by_title(self,title):
        #http://stackoverflow.com/questions/10320633/mongo-how-to-query-a-nested-json
        #db.books.find({ 'best_book.title' : 'Unicorn Variations'})
        #self.db.books.find_one({'best_book.title' : title})
        #where = "/.*" + title + ".*/"
        #return self.db.books.find_one({'best_book.title' : where})
        #http://docs.mongodb.org/manual/reference/operator/query/regex/#op._S_regex
        return self.db.books.find_one({'best_book.title':{'$regex':title , '$options':'i'}})

    def find_books_by_author(self,author):
        return self.db.books.find({'best_book.author.name':{'$regex':author , '$options':'i'}})

    def delete(self,title):
        #http://api.mongodb.org/python/current/api/pymongo/collection.html#pymongo.collection.Collection.find_and_modify
        self.db.books.find_and_modify(query={ 'best_book.title':{'$regex':title , '$options':'i'} }, remove=True)

    def find_books_by_sqlid(self,sqlid):
        if type(sqlid) != int:
            sqlid = int(sqlid)
        return self.db.books.find_one({'sqlid':sqlid})

    def update_book_with_sqlid(self,sqlid,jsonobj):
        bk = self.find_books_by_sqlid(sqlid)
        self.db.books.update({"_id":bk["_id"]},{"$set": jsonobj})

    def find_books_with_zero_sqlid(self):
        return self.db.books.find({'sqlid':0})

    def update_book_sqlid(self,mongo_id,sqlid):
        self.db.books.update({"_id":mongo_id},{"$set":{"sqlid":int(sqlid)}},upsert=False,multi=False)

    def delete_by_mongoid(self,mid):
        self.db.books.find_and_modify(query={'_id': mid}, remove=True)

    def delete_by_sqlid(self,sid):
        self.db.books.find_and_modify(query={'sqlid': sid}, remove=True)

if __name__ == '__main__':
    #http://api.mongodb.org/python/current/tutorial.html
    #mongo must be running in order for this to work:  mongod --config /usr/local/etc/mongod.conf
    #something here
    ma = MongoAdmin()
    #print(ma.test())
    #book = ma.find_book_by_title('Unicorn Variations')
    #print(book['best_book'][0]['title'][0])
    #print(ma.searchby({"book_sqlid":1}))

    #print(ma.db.books.find_one({'sqlid':1}))

    '''
    sid = 3812
    rs = ma.find_books_by_sqlid(sid)
    print(rs)
    rs['best_book'][0]['image_url'][0] = "https://d.gr-assets.com/books/1320399351l/1885.jpg"
    del(rs['_id'])
    ma.update_book_with_sqlid(sid,rs)
    '''
    #book = ma.find_book_by_title('Unicorn Variation')
    #print(book)
    #print(rs)
    ma.delete_by_sqlid(8192)

    #book = ma.find_book_by_title("DragonFly in Amber")
    #print(book)

    #ma.delete("DragonFly in Amber")

    #books = ma.find_books_by_author("Diana Gabaldon")
    #for b in books:
        #print(b)
        #print(b['best_book'][0]['title'][0] + "(")


