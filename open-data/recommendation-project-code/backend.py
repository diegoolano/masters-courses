import csv
import json
from flask import Flask
from flask.ext.sqlalchemy import SQLAlchemy

app = Flask(__name__)
#app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://root:root@localhost:8889/opendata'
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://root@localhost/recdb'
db = SQLAlchemy(app)

class Book(db.Model):
    __tablename__ = 'books'
    id = db.Column(db.Integer, primary_key=True)
    title = db.Column(db.String)
    #author = db.Column(db.String)

class User(db.Model):
    __tablename__ = 'users'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String)


class Rating(db.Model):
    __tablename__ = 'ratings'
    bookid = db.Column(db.Integer, db.ForeignKey('books.id'), primary_key=True)
    userid = db.Column(db.Integer, db.ForeignKey('users.id'), primary_key=True)
    rating = db.Column(db.Integer, default=5)
    book = db.relationship(Book)
    user = db.relationship(User)

class RatingBinary(db.Model):
    __tablename__ = 'ratings_binary'
    bookid = db.Column(db.Integer, db.ForeignKey('books.id'), primary_key=True)
    userid = db.Column(db.Integer, db.ForeignKey('users.id'), primary_key=True)
    rating = db.Column(db.Integer, default=5)
    book = db.relationship(Book)
    user = db.relationship(User)

class Backend():
    def __init__(self):
        self.book_table = Book()
        self.user_table = User()
        self.rating_table = Rating()
        self.rating_binary_table = RatingBinary()

    def insert_book(self,booktitle):
        s = db.session()
        b = Book()
        b.title = booktitle
        s.add(b)
        s.commit()
        return self.book_table.query.order_by("-id").first()

    def insert_rating(self,userid,bookid,rating):
        s = db.session()
        b = Rating()
        b.bookid = bookid
        b.userid = userid
        b.rating = rating
        s.add(b)
        s.commit()

    def insert_binary_rating(self,userid,bookid,rating):
        s = db.session()
        b = RatingBinary()
        b.bookid = bookid
        b.userid = userid
        b.rating = rating
        s.add(b)
        s.commit()


    def get_book_by_title(self,booktitle):
        return self.book_table.query.filter_by(title=booktitle).first()

    def get_book_by_author(self,bookauthor):
        return self.book_table.query.filter_by(author=bookauthor).first()

    def get_book_with_title_like(self,bt):
        t = '%'+bt+'%'
        return Book.query.filter(Book.title.like(t)).all()

    def searchby(self,request):
        fieldname = request.keys()[0]
        if fieldname == "title":
            return self.get_book_by_title(request[fieldname])
        elif fieldname == "author":
            return self.get_book_by_author(request[fieldname])
        else:
            return "request not valid"

    def get_ratings_for_user(self,uid,ratingstype):
        if ratingstype == "decimal":
            return self.rating_table.query.filter_by(userid=uid).order_by("bookid").all()
        else:
            return self.rating_binary_table.query.filter_by(userid=uid).order_by("bookid").all()

    def get_ratings_for_book(self,bid,ratingstype):
        if ratingstype == "decimal":
            return self.rating_table.query.filter_by(bookid=bid).all()
        else:
            return self.rating_binary_table.query.filter_by(bookid=bid).all()

    def get_ratings_for_user_book(self,uid,bid,ratingstype):
        if ratingstype == "decimal":
            return self.rating_table.query.filter_by(bookid=bid,userid=uid).first()
        else:
            return self.rating_binary_table.query.filter_by(bookid=bid,userid=uid).first()

    def get_user_book_ratings_json(self,rating_type="decimal"):
        #for each user
        #make object of each book they've rated along with rating
        if rating_type == "decimal":
            print("IN USER BOOK ratings decimal")
            users = self.rating_table.query.order_by("userid","bookid").all()
        else:
            users = self.rating_binary_table.query.order_by("userid","bookid").all()

        ret = {}
        for user in users:
            bid = user.bookid
            rat = user.rating
            if user.userid in ret:
                ret[user.userid][bid] = rat
            else:
                ret[user.userid] = {bid:rat}

        return ret

    def get_book_user_ratings_json(self,rating_type="decimal"):
              #for each user
        #make object of each book they've rated along with rating
        if rating_type == "decimal":
            books = self.rating_table.query.order_by("bookid","userid").all()
        else:
            books = self.rating_binary_table.query.order_by("bookid","userid").all()

        ret = {}
        for book in books:
            uid = book.userid
            rat = book.rating
            if book.bookid in ret:
                ret[book.bookid][uid] = rat
            else:
                ret[book.bookid] = {uid:rat}

        return ret

    def initialize_db(self):
        return "already done"
        #handle books table
        """"
        bookstsv = "/Users/diego/htdocs/opendata/mongo-titleauthor.tsv"
        bookssql = "/Users/diego/htdocs/opendata/books-info.sql"
        with open(bookstsv,'rb') as tsvin, open(bookssql, "w") as booksout:
            tsvin = csv.reader(tsvin, delimiter='\t')

            for row in tsvin:
                title = row[0]
                author = row[1]
                booksout.write('INSERT into `books` (title, author) VALUES ("'+title+'","'+author+'");\n')

        booksout.close()


        #handle ratings table
            #> summary(ratings)
             # DBbook_userID  DBbook_itemID       rate
             # Min.   :   1   Min.   :   1   Min.   :0.000
             # 1st Qu.:1787   1st Qu.:2056   1st Qu.:3.000
             # Median :3620   Median :4233   Median :4.000
             # Mean   :3620   Mean   :4157   Mean   :3.875
             # 3rd Qu.:5433   3rd Qu.:6256   3rd Qu.:5.000
             # Max.   :7255   Max.   :8169   Max.   :5.000
        ratingstsv = "/Users/diego/htdocs/opendata/DBbook_train_ratings.tsv"
        ratingssql = "/Users/diego/htdocs/opendata/ratings-info.sql"
        with open(ratingstsv,'rb') as tsvin, open(ratingssql, "w") as ratingsout:
            tsvin = csv.reader(tsvin, delimiter='\t')

            for row in tsvin:
                ratingsout.write('INSERT into `ratings` (bookid, userid, rating) VALUES ('+row[0]+','+row[1]+','+row[2]+');\n')

        ratingsout.close()

        #handle users table
        userssql = "/Users/diego/htdocs/opendata/users-info.sql"
        with open(userssql, "w") as usersout:
            for i in range(7256):
                usersout.write('INSERT into `users` (name) VALUES ("user'+str(i)+'");\n')

        usersout.close()
        """""


if __name__ == '__main__':
    b = Backend()
    #sqlid = b.insert_book("Test Book4")
    #print(sqlid.id)
    #b.initialize_db()

    cnt = b.get_user_book_ratings_json()
    print(cnt.keys())

    print(cnt[1])
    #cnt2 = b.get_book_user_ratings_json()
    #print(cnt2.keys())

    #print(b.get_book_by_title("Unicorn Variations").id)

    #userbrt = "/Users/diego/htdocs/opendata/user-book-rating-testing.json"
    #with open(userbrt,"w") as wout:
    #    wout.write(json.dumps(b.get_user_book_ratings_json("binary")))

    #NOTES

    #local mysql dependency
    #start mysqld in one tab
    # /usr/local/mysql-5.1.42-osx10.5-x86_64/bin/mysqld
    #start mysql in another
    #/usr/local/mysql-5.1.42-osx10.5-x86_64/bin/mysql -h localhost -uroot
    #CREATE DATABASE opendata;
    #exit
    # import in database
    #/usr/local/mysql-5.1.42-osx10.5-x86_64/bin/mysql -h localhost -uroot opendata < /Users/diego/htdocs/opendata/opendata2.sql


    #python dependencies
    #http://stackoverflow.com/questions/2952187/getting-error-loading-mysqldb-module-no-module-named-mysqldb-have-tried-pre
    #/Applications/MAMP/Library/bin/mysql
    #export PATH=$PATH:/Applications/MAMP/Library/bin/
    #sudo pip -E install MySQL-python

    #sql alchemy - webpy (or flask)
    #sudo pip install SQLAlchemy
    #sudo pip install flask
    #sudo pip install flask-sqlalchemy

    #reading
    #http://www.jeffknupp.com/blog/2014/01/18/python-and-flask-are-ridiculously-powerful/
    #https://pythonhosted.org/Flask-SQLAlchemy/quickstart.html

