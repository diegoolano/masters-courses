from json import load, dumps
from mongo_admin import *
from backend import *
from goodreads import *
from quadstore_admin import *
from dbpedia import *
from recommendations import *
from sqlalchemy import func

from flask import Flask, render_template, request as FlaskRequest, session, g, redirect, url_for,flash
app = Flask(__name__)

class Middleware:
    def __init__(self):
        self.config_file = "/Users/diego/PycharmProjects/opendata/metadata/config.json"
        #self.config_file = "/Users/diego/htdocs/opendata/config.json"
        self.config = self.load_config()
        self.qa = QuadstoreAdmin()
        self.rec = Recommendations()
        self.bck = Backend()
        self.m = MongoAdmin()
        self.head = self.sethead()

    def sethead(self):
        return '<head><link rel="stylesheet" type="text/css" href="/static/theme.css"/><script src="/static/jquery.min.js"></script><script src="/static/app.js"></script></head>'

    def get_page_head(self,userid,ratingstype):
        print(userid)
        if userid != None and str(userid) != '0':
            userarea = "<div class='login'><span class='usertop'>Hi User "+str(userid)+"!</span> | <a href='/profile'>see profile</a> | <a href='/'>home </a> | <a href='javascript: toggle_login();'>logout</a>"
        else:
            userarea = "<div class='login'><a href='/'>home </a> | <a href='javascript: toggle_login();'>login</a>"

        if ratingstype == "decimal":
            userarea += "<br>mode: <span class='blue'>decimal</span> | <a href='javascript: switch_to_binary()'>binary</a> </div>"
        else:
            userarea += "<br>mode: <a href='javascript: switch_to_decimal()'>decimal</a> | <span class='blue'>binary</span> </div>"

        results = "<html>"+self.head+"<body><div id='header'><h1>META BOOKS</h1>"+userarea+"</div>"
        results += "<div id='search'><form action='http://127.0.0.1:5000/searchby/' method='GET' id='headerSearchForm'><input id='titlesearch' type='text' placeholder='search by title' name='titlesearch'/>"
        results += '<a class="submitLink" href="#" onclick=""><img alt="search" src="https://s.gr-assets.com/assets/layout/magnifying_glass-f6b50ce529bb372c2503aa1c685a2505.png" title="Title" width="16"></a></form></div>'
        return results


    #this function is used when a search returns many results and our sources don't agree definitively on a single one
    #in this case, we return a list of books to the end user for them to select from
    #and once selected that book is incorporated into our system locally ( as opposed to the others which remain external )
    def show_for_confirmation(self,content,searchterm):
        #go through external sources to show
        externals = self.config['external_sources']
        frontend_wants = self.config['frontend']['metadata_to_display']
        search_rules = self.config['searchrules']
        final_books = []

        for i in externals:
            if search_rules['dbpedia_as_master_external_source'] == True:
                if i != 'dbpedia':
                    continue

            print("going for "+i)
            current = content[i]['val']
            print(current)
            if current == None or current == [] or current == {}:
                print("empty")
            else:

                #at this point current is a list of dbpedia items
                for currento in current:
                    print(currento)
                    merged_result = {}
                    obj = currento
                    mps = externals[i]['mapping']
                    mps["uri"]=""  #include uri
                    depth = externals[i]['mapping_depth']
                    index = externals[i]['index']
                    print("depth" + str(depth))
                    print("index" + str(index))


                    for m in mps:
                        #m is frontend term
                        #mps[m] is term in external source
                        print(m + " --> " + mps[m])
                        #exm = mps[m]
                        exm = m

                        try:
                            if depth == 0:
                                if "." in exm and "http" not in exm:
                                    #to allow nesting within map.. ie, bestbook.authorName
                                    ps = exm.split('.')
                                    p1 = obj[ps[0]]
                                    if type(p1) == list:
                                        val = p1[0][ps[1]]
                                    else:
                                        val = p1[ps[1]]

                                    if type(val) == list:
                                        val = val[0]

                                else:
                                    val = obj[exm]
                            else:
                                print("--------------------DEPTH1-----------")
                                print(exm)
                                print(obj)
                                root = obj.keys()
                                print(root)
                                if "." in exm and "http" not in exm:
                                    #to allow nesting within map.. ie, bestbook.authorName
                                    ps = exm.split('.')
                                    p1 = obj[root][ps[0]]
                                    if type(p1) == list:
                                        val = p1[0][ps[1]]
                                    else:
                                        val = p1[ps[1]]

                                    if type(val) == list:
                                        val = val[0]
                                else:
                                    val = obj[exm]
                                    print(val)

                            if index != "":
                                val = val[index]
                        except:
                            val = ""

                        print("VAL is "+str(val))
                        merged_result[m] = val
                    final_books.append(merged_result)

        print("finalbooks")
        print(final_books)
        userid = FlaskRequest.cookies.get('userid')
        ratingstype = FlaskRequest.cookies.get('ratingstype')
        if ratingstype == None:
            ratingstype = "decimal"

        results = self.get_page_head(userid,ratingstype)
        results += "<div id='profile'>External Search Results for <span class='blue'>"+searchterm+"</span><br/></div><div id='content'>"
        index = 0
        if final_books == []:
            results += "<span class='green'>None found!</span>"
        else:
            for f in final_books:
                if 'cover_image' not in f:
                    f['cover_image'] = "http://s.gr-assets.com/assets/nophoto/book/111x148-6204a98ba2aba2d1aa07b9bea87124f8.png"
                elif f['cover_image'] == "":
                    f['cover_image'] = "http://s.gr-assets.com/assets/nophoto/book/111x148-6204a98ba2aba2d1aa07b9bea87124f8.png"

                #make sure that the frontend displays are at least null
                for findex in frontend_wants:
                    if findex not in f:
                        f[findex] = ""

                if "name" in f["author"]:
                    f["author"] = f["author"]["name"]
                print(f)
                #construct template
                thisbook = "<div id='"+ str(index) +"' class='book'>"+\
                                "<a href='/confirm/"+f['title']+"'><img src='"+f['cover_image']+"'/></a>"+\
                                "<h2>"+f['title'].replace('"',"")+"</h2><table>"+\
                                "<tr><td>author:</td><td>"+f['author']+"</td></tr>"+\
                                "<tr><td>year:</td><td>"+str(f['year_of_publication'])+"</td></tr></table>"+\
                                "<br/><form action='http://127.0.0.1:5000/confirm' method='POST' id='confirmbookForm'>"+\
                                "Click submit if this is your book<input type='hidden' name='uri' value='"+f['uri']+"'/>"+\
                                "<input type='hidden' name='title' value='"+f['title']+"'/><input type='hidden' name='author' value='"+f['author']+"'/><input type='submit' value='Submit'></form>"+\
                                "</div>"
                index = index + 1
                results += thisbook

        results += "</div></body></html>"
        return results


    #this function is used to display multiple books which are returned locally (ie, from mysql db)
    #for a given search.  it includes a link for the user to select from external sources if the result they want is not local already
    def show_list_of_books(self,bids,searchterm):
        userid = FlaskRequest.cookies.get('userid')
        ratingstype = FlaskRequest.cookies.get('ratingstype')
        if ratingstype == None:
            ratingstype = "decimal"

        results = self.get_page_head(userid,ratingstype)
        results += "<div id='profile'>Search Results for <span class='blue'>"+searchterm+"</span><br/><span class='italic'>if its not one of these click <a href='/searchexternal/?titlesearch="+searchterm+"'>here to search externally</a></span></div><div id='content'>"

        for bobj in bids:
            print(bobj)
            bid = bobj.id
            print(bid)
            maxnum = 24
            ratings = self.bck.get_ratings_for_book(bid,ratingstype)

            nb = 0
            sum = 0
            if ratings == []:
                bookingrating = "None"
            else:
                for r in ratings:
                    sum += r.rating
                    nb += 1
                ra = float(sum)/nb
                bookrating = ra * (5+nb/maxnum)/6
                bookrating = "%.2f" % round(bookrating,2)

            merged = self.get_book_content(bid)

            if userid != None or userid != 0:
                ur = self.bck.get_ratings_for_user_book(userid,bid,ratingstype)
                ratingarea = self.ratingsmap(ur,ratingstype,bid,userid)

            if merged['cover_image'] == "":
                merged['cover_image'] = "http://s.gr-assets.com/assets/nophoto/book/111x148-6204a98ba2aba2d1aa07b9bea87124f8.png"

            #construct template
            thisbook = "<div id='"+ str(bid) +"' class='book'>"+\
                            "<a href='/book/"+str(bid)+"'><img src='"+merged['cover_image']+"'/></a>"+\
                            "<h2>"+merged['title'].replace('"',"")+"</h2>"+ratingarea+"<table>"+\
                            "<tr><td>author:</td><td>"+merged['author']+"</td></tr>"+\
                            "<tr><td>year:</td><td>"+merged['year_of_publication']+"</td></tr>"+\
                            "<tr><td>country:</td><td>"+merged['country'].split('/')[-1].replace("_"," ")+"</td></tr>"+\
                            "<tr><td>rating:</td><td><span class='rating'>"+ bookrating +"</span> <span class='numreviews'>("+str(nb)+" reviews)</span></td></tr></table></div>"

            results += thisbook

        #construct template with content
        results += "</div></body></html>"
        return results


    #this function returns all the books a logged in user has rated, along with their ratings.
    def profile(self):
        userid = FlaskRequest.cookies.get('userid')
        ratingstype = FlaskRequest.cookies.get('ratingstype')
        if ratingstype == None:
            ratingstype = "decimal"
        ratings = self.bck.get_ratings_for_user(userid,ratingstype)
        results = self.get_page_head(userid,ratingstype)
        results += "<div id='profile'>Profile for User: "+str(userid)+"</div><div id='content'>"
        for rating in ratings:
            bid = rating.bookid
            r = "%.2f" % round(rating.rating,2)
            merged = self.get_book_content(bid)

            if merged['cover_image'] == "":
                merged['cover_image'] = "http://s.gr-assets.com/assets/nophoto/book/111x148-6204a98ba2aba2d1aa07b9bea87124f8.png"

            thisbook = "<div id='"+ str(bid) +"' class='book'>"+\
                            "<a href='/book/"+str(bid)+"'><img src='"+merged['cover_image']+"'/></a>"+\
                            "<h2>"+merged['title'].replace('"',"")+"</h2><table>"+\
                            "<tr><td>YOUR RATING</td><td><span class='rating'>"+ r +"</span></td></tr>"+\
                            "<tr><td>author:</td><td>"+merged['author']+"</td></tr>"+\
                            "<tr><td>year:</td><td>"+merged['year_of_publication']+"</td></tr>"+\
                            "<tr><td>country:</td><td>"+merged['country'].split('/')[-1].replace("_"," ")+"</td></tr></table></div>"

            results += thisbook
        #construct template with content
        results += "</div></body></html>"
        return results


    #this function given a local book id ( sqlid) returns the merged content from our local repositories (mongodb / 4store)
    def get_book_content(self,bid):
        print("call search by with book: "+str(bid))
        rdf_data = self.qa.searchID(str(bid))

        print("call mongodb with book: "+str(bid))
        json_data = self.m.find_books_by_sqlid(bid)

        #merge these two using rules
        merged = self.process_single_item_mapping({'dbpedia':rdf_data, 'goodreads':json_data})
        return merged


    #a silly helper function to personalize ratings for user a bit :)
    def ratingsmap(self,ur,ratingstype,bid,userid):
            if ur != None:
                #show user rating
                if ratingstype == "decimal":
                    if ur.rating == 1: thought = "sucked bad (1 out of 5)"
                    if ur.rating == 2: thought = "sort of sucked (2 out of 5)"
                    if ur.rating == 3: thought = "was ok (3 out of 5)"
                    if ur.rating == 4: thought = "was awesome (4 out of 5)"
                    if ur.rating == 5: thought = "was way awesome (5 out of 5)"
                else:
                    if ur.rating == 0:
                        thought = "a thumbs down"
                    else:
                        thought = "a thumbs up"

                ratingarea = "<div id='ratingarea'>You thought it <span class='thoughts'>"+thought+"</span></div>"
            else:
                #allow user to rate it
                if ratingstype == "decimal":
                    ratingarea = "<div id='ratingarea'><form action='http://127.0.0.1:5000/rate' method='GET' id='headerSearchForm'>rate it: " \
                                 +"<input type='radio' name='rating' value=1>1"\
                                 +"<input type='radio' name='rating' value=2>2"\
                                 +"<input type='radio' name='rating' value=3>3"\
                                 +"<input type='radio' name='rating' value=4>4"\
                                 +"<input type='radio' name='rating' value=5>5"\
                                 +"<input type='hidden' name='bookid' value="+str(bid)+">"\
                                 +"<input type='hidden' name='userid' value="+str(userid)+">"\
                                 +"<input type='hidden' name='ratingstype' value='decimal'><input type='submit' value='Submit'></form></div>"

                else:
                    ratingarea = "<div id='ratingarea'><form action='http://127.0.0.1:5000/rate' method='GET' id='headerSearchForm'>rate it: " \
                                 +"<input type='radio' name='rating' value=0>0"\
                                 +"<input type='radio' name='rating' value=1>1"\
                                 +"<input type='hidden' name='bookid' value="+str(bid)+">"\
                                 +"<input type='hidden' name='userid' value="+str(userid)+">"\
                                 +"<input type='hidden' name='ratingstype' value='binary'><input type='submit' value='Submit'></form></div>"
            return ratingarea


    #this function returns the page view for a given local book id
    #it also gets user based recommendations based on the book
    #and additonally content based recommendations using linked data in our local 4store

    def singlebook(self,bid, external_search_term=0):
        print("SINGLE BOOK")
        userid = FlaskRequest.cookies.get('userid')
        ratingstype = FlaskRequest.cookies.get('ratingstype')
        if ratingstype == None:
            ratingstype = "decimal"

        results = self.get_page_head(userid,ratingstype)

        #select bookid,count(userid) as ratingscount from ratings group by bookid order by ratingscount desc limit 20;
        #this is a little hack to optimize getting book rating (which is dependent on knowing how many times the most rated book has been rated)
        #in real life, this information would be precomputed and stored for quick lookup
        maxnum = 24
        ratings = self.bck.get_ratings_for_book(bid,ratingstype)
        nb = 0
        sum = 0
        if ratings == []:
            bookrating = "None"
        else:
            for r in ratings:
                sum += r.rating
                nb += 1
            ra = float(sum)/nb
            bookrating = ra * (5+nb/maxnum)/6
            bookrating = "%.2f" % round(bookrating,2)

        #check if user logged in, and if so, if he has rated this book already
        ratingarea = ""
        if str(userid) != '0' and userid != None:
            print(str(userid))
            print(ratingstype)

            ur = self.bck.get_ratings_for_user_book(userid,bid,ratingstype)
            ratingarea = self.ratingsmap(ur,ratingstype,bid,userid)


        merged = self.get_book_content(bid)
        if external_search_term == 0:
            results += "<div id='pagetitle'>Book: "+merged['title'].replace('"',"")+"</div>"
        else:
            results += "<div id='pagetitle'>Book: "+merged['title'].replace('"',"")+" (<span class='italic'>if this is not the book you want <a href='/searchexternal/?titlesearch="+external_search_term+"'>click here</a> to search externally</a></span>)</div>"

        if merged['cover_image'] == "":
            merged['cover_image'] = "http://s.gr-assets.com/assets/nophoto/book/111x148-6204a98ba2aba2d1aa07b9bea87124f8.png"

        #construct template
        thisbook = "<div id='"+ str(bid) +"' class='book single'>"+\
                            "<a href='/book/"+str(bid)+"'><img src='"+merged['cover_image']+"'/></a>"+\
                            "<h2>"+merged['title'].replace('"',"")+"</h2>"+ratingarea+"<table>"+\
                            "<tr><td>author:</td><td>"+merged['author']+"</td></tr>"+\
                            "<tr><td>year:</td><td>"+merged['year_of_publication']+"</td></tr>"+\
                            "<tr><td>country:</td><td>"+merged['country'].split('/')[-1].replace("_"," ")+"</td></tr>"+\
                            "<tr><td>rating:</td><td><span class='rating'>"+ bookrating +"</span> <span class='numreviews'>("+str(nb)+" reviews)</span></td></tr></table></div>"

        results += thisbook
        #construct template with content
        results += "</div>"

        #GETTING RECOMMENDATIONS for SIMILAR books by USERS AND SIMILAR CONTENT by RDF
        #Collaborative

        collab_cnt = []

        #find other books similar to this one based on bid
        data = self.bck.get_book_user_ratings_json(ratingstype)
        print("bid "+str(bid))
        print(data.keys())
        #print(data[str(bid)])
        if long(bid) not in data.keys():
            #book hasn't been reviewed yet
            collab_cnt = []
        else:
            collab_cnt  = self.rec.topMatches(data,long(bid),similarity='sim_distance')

        print("collab cnt for bid("+str(bid)+")")
        print(collab_cnt)

        pagebid = bid
        collab_ids = []

        if collab_cnt != []:

            results += "<div id='collab_content'><h1>Users who liked this also like ... (collaborative recommendation)</h1>"

            for bobj in collab_cnt:
                bid = bobj[1]

                collab_ids.append(bid)
                print("---------------------Get content for book: "+str(bid))
                maxnum = 24
                ratings = self.bck.get_ratings_for_book(bid,ratingstype)
                print("get ratings")
                nb = 0
                sum = 0
                if ratings == []:
                    bookingrating = "None"
                else:
                    for r in ratings:
                        sum += r.rating
                        nb += 1
                    ra = float(sum)/nb
                    bookrating = ra * (5+nb/maxnum)/6
                    bookrating = "%.2f" % round(bookrating,2)

                merged = self.get_book_content(bid)

                ratingarea = ""
                if userid != None and str(userid) != '0':
                    ur = self.bck.get_ratings_for_user_book(userid,bid,ratingstype)
                    ratingarea = self.ratingsmap(ur,ratingstype,bid,userid)

                if merged['cover_image'] == "":
                    merged['cover_image'] = "http://s.gr-assets.com/assets/nophoto/book/111x148-6204a98ba2aba2d1aa07b9bea87124f8.png"

                #construct template
                thisbook = "<div id='"+ str(bid) +"' class='book'>"+\
                                "<a href='/book/"+str(bid)+"'><img src='"+merged['cover_image']+"'/></a>"+\
                                "<h2>"+merged['title'].replace('"',"")+"</h2>"+ratingarea+"<table>"+\
                                "<tr><td>author:</td><td>"+merged['author']+"</td></tr>"+\
                                "<tr><td>year:</td><td>"+merged['year_of_publication']+"</td></tr>"+\
                                "<tr><td>country:</td><td>"+merged['country'].split('/')[-1].replace("_"," ")+"</td></tr>"+\
                                "<tr><td>rating:</td><td><span class='rating'>"+ bookrating +"</span> <span class='numreviews'>("+str(nb)+" reviews)</span></td></tr></table></div>"

                results += thisbook
        else:
            #no reviews
            results +="<span class='green'>No reviews for book yet</span>"
            results += "</div>"

        print("Collab_IDs")
        print(str(collab_ids))

        bid = pagebid
        print 'Content based -------------------------------------'
        current = self.config['searchrules']['similarity']['current']
        content_based = {}
        if "." in current:
            cs = current.split(".")
            max_books = 8/len(cs)
            for c in cs:
                content_based[c] = []
                thisctr = self.config['searchrules']['similarity']['criteria'][c]
                print(c)
                print(thisctr)
                content_cnt = self.qa.searchSimilar(thisctr, str(bid))
                count = 1
                for book in content_cnt:
                    print("BOOK: "+str(book))
                    if count <= max_books:
                        currentbid = content_cnt[book]['http://rdf.recommender/id'][0]['value']
                        if currentbid not in content_based[c] and long(currentbid) not in collab_ids and str(currentbid) != str(pagebid):
                            print("add "+str(currentbid)+" to content_based")
                            content_based[c].append(currentbid)
                            count = count + 1
                        else:
                            print("found "+str(currentbid)+" already in collab content/main bid so don't add it")

        else:
            cs = self.config['searchrules']['similarity']['criteria'][current]
            content_cnt = self.qa.searchSimilar(cs, str(bid))
            count = 1
            max_books = 4
            for book in content_cnt:
                if count <= max_books:
                    currentbid = content_cnt[book]['http://rdf.recommender/id'][0]['value']
                    if currentbid not in content_based and long(currentbid) not in collab_ids and str(currentbid) != str(bid):
                        content_based.append(currentbid)
                        count = count + 1

        print("content based results: ")
        print(content_based)

        if content_based != {}:
            for cob in content_based.keys():

                results += "<div class='content_based'><h1>Books similar to this book according to <span class='blue'>"+cob+"</span></h1>"
                if content_based[cob] == []:
                    results += "<span class='green'>No other books with same "+cob+" found locally</span>"

                for bobj in content_based[cob]:
                    bid = bobj
                    print("---------------------Get content for book: "+str(bid))
                    maxnum = 24
                    ratings = self.bck.get_ratings_for_book(bid,ratingstype)
                    print("RATINGS")
                    nb = 0
                    sum = 0
                    if ratings == []:
                        bookingrating = "None"
                    else:
                        for r in ratings:
                            sum += r.rating
                            nb += 1
                        ra = float(sum)/nb
                        bookrating = ra * (5+nb/maxnum)/6
                        bookrating = "%.2f" % round(bookrating,2)

                    merged = self.get_book_content(bid)

                    ratingarea = ""
                    if userid != None and str(userid) != '0':
                        ur = self.bck.get_ratings_for_user_book(userid,bid,ratingstype)
                        ratingarea = self.ratingsmap(ur,ratingstype,bid,userid)

                    if merged['cover_image'] == "":
                        merged['cover_image'] = "http://s.gr-assets.com/assets/nophoto/book/111x148-6204a98ba2aba2d1aa07b9bea87124f8.png"

                    #construct template
                    thisbook = "<div id='"+ str(bid) +"' class='book'>"+\
                                    "<a href='/book/"+str(bid)+"'><img src='"+merged['cover_image']+"'/></a>"+\
                                    "<h2>"+merged['title'].replace('"',"")+"</h2>"+ratingarea+"<table>"+\
                                    "<tr><td>author:</td><td>"+merged['author']+"</td></tr>"+\
                                    "<tr><td>year:</td><td>"+merged['year_of_publication']+"</td></tr>"+\
                                    "<tr><td>country:</td><td>"+merged['country'].split('/')[-1].replace("_"," ")+"</td></tr>"+\
                                    "<tr><td>rating:</td><td><span class='rating'>"+ bookrating +"</span> <span class='numreviews'>("+str(nb)+" reviews)</span></td></tr></table></div>"

                    results += thisbook
            results += "</div>"

        results += "</body></html>"

        #print(results)
        return results


    #this function returns the home page of the site ( for either loggedin / or not case)
    def home(self):
        userid = FlaskRequest.cookies.get('userid')
        ratingstype = FlaskRequest.cookies.get('ratingstype')
        if ratingstype == None:
            ratingstype = "decimal"

        content = self.get_cold_start(userid,ratingstype)

        #get top 8 books and display
        books = content[0:8]
        print(books)

        results = self.get_page_head(userid,ratingstype)
        if userid == None or str(userid) == '0':
            title = "<div id='pagetitle'>Top rated books</div>"
        else:
            title = "<div id='pagetitle'>Based on your ratings history, books you may like ...</div>"

        results += title + "<div id='content'>"

        for book in books:
            #given books as objects {rating,numreviews,bookid}
            bookrating = "%.2f" % round(book[0],2)
            numreviews = str(book[1])
            bid = book[2]
            merged = self.get_book_content(bid)

            if merged['cover_image'] == "":
                merged['cover_image'] = "http://s.gr-assets.com/assets/nophoto/book/111x148-6204a98ba2aba2d1aa07b9bea87124f8.png"

            #construct template
            thisbook = "<div id='"+ str(bid) +"' class='book'>"+\
                            "<a href='/book/"+str(bid)+"'><img src='"+merged['cover_image']+"'/></a>"+\
                            "<h2>"+merged['title'].replace('"',"")+"</h2><table>"+\
                            "<tr><td>author:</td><td>"+merged['author']+"</td></tr>"+\
                            "<tr><td>year:</td><td>"+merged['year_of_publication']+"</td></tr>"+\
                            "<tr><td>country:</td><td>"+merged['country'].split('/')[-1].replace("_"," ")+"</td></tr>"+\
                            "<tr><td>rating:</td><td><span class='rating'>"+ bookrating +"</span> <span class='numreviews'>("+numreviews+" reviews)</span></td></tr></table></div>"

            results += thisbook
        #construct template with content
        results += "</div></body></html>"
        return results


    def get_cold_start(self,userid=None,ratingstype="decimal"):
        print("called cold start for ")
        print("userid: "+str(userid))
        print("ratingstype: "+str(ratingstype))

        if userid == None or str(userid) == '0':
            #cold start not logged in
            print("not logged in")
            data = self.bck.get_book_user_ratings_json(ratingstype)
            return self.rec.best_books(data)
        else:
            #cold start logged in
            print("logged in as "+str(userid))


            uratings = self.bck.get_ratings_for_user(userid,ratingstype)
            print("Uratings")
            print(uratings)
            if len(uratings) == 0:
                #if no ratings yet, return normal home page
                data = self.bck.get_book_user_ratings_json(ratingstype)
                return self.rec.best_books(data)


            data = self.bck.get_user_book_ratings_json(ratingstype)

            if len(uratings) < 5:
                #for users with few ratings have to use this
                print("euclidean")
                cnt = self.rec.getRecommendations(data,int(userid),similarity='sim_distance')
                return cnt


            if ratingstype == "decimal":
                #use pearson for decimal
                print("sim_pearson")
                cnt = self.rec.getRecommendations(data,int(userid),similarity='sim_pearson')

            else:
                #use euclidean for binary
                print("euclidean")
                cnt = self.rec.getRecommendations(data,int(userid),similarity='sim_distance')
            return cnt


    #this does the actual mapping between our sources!!
    #it includes many kind of ugly safeguards, which is still generalizable, but due to state of goodreads api
    def process_single_item_mapping(self,result):
        merged_result = {}
        frontend_wants = self.config['frontend']['metadata_to_display']
        externals = self.config['external_sources']
        backend_has = {}
        for i in externals:
            backend_has[i] = externals[i]['mapping']

        print("Frontend wants: "+json.dumps(frontend_wants))
        print("External sources have: "+ json.dumps(externals))
        print("Merging: "+ str(result))

        for s in result.keys():
            obj = result[s]
            mps = externals[s]['mapping']
            depth = externals[s]['mapping_depth']
            index = externals[s]['index']

            for m in mps:
                #m is frontend term
                #mps[m] is term in external source
                #print(m + " --> " + mps[m])
                exm = mps[m]
                try:
                    if depth == 0:
                        if "." in exm and "http" not in exm:
                            #to allow nesting within map.. ie, bestbook.authorName
                            ps = exm.split('.')
                            p1 = obj[ps[0]]
                            if type(p1) == list:
                                val = p1[0][ps[1]]
                            else:
                                val = p1[ps[1]]

                            if type(val) == list:
                                val = val[0]

                        else:
                            val = obj[exm]
                    else:
                        root = obj.keys()[0]
                        if "." in exm and "http" not in exm:
                            #to allow nesting within map.. ie, bestbook.authorName
                            ps = exm.split('.')
                            p1 = obj[root][ps[0]]
                            if type(p1) == list:
                                val = p1[0][ps[1]]
                            else:
                                val = p1[ps[1]]

                            if type(val) == list:
                                val = val[0]
                        else:
                            val = obj[root][exm]

                    if index != "":
                        val = val[0][index]
                except:
                    val = ""

                if m not in merged_result:
                    #this is to enforce precedence of sources as set in config.json
                    merged_result[m] = val

        print("---merged result: ")
        print(merged_result)
        return merged_result


    #load config file
    def load_config(self):
        conf = open(self.config_file)
        return json.load(conf)

    #get config file
    def get_config(self):
        return self.config

    #this is the main function used to search!
    def search_request(self,request):
        #request expected to be json object of the form {"field":"value"}

        #load rules from config
        search_rules = self.config['searchrules']

        if search_rules['check_for_local_version_first'] == True:
            print("checking locally first")
            r = self.bck.searchby(request)
            if r == None or r == "request not valid":
                #go to APIS !!
                print("found nothing so go to external apis")
                final_results = self.call_external_sources(request)
            else:
                print("founding something local so returning it")
                #get mongoinfo
                metamongo = self.m.searchby(request)

                #get rdfinfo
                metardf = self.qa.searchID(str(r.id))

                results = {'goodreads':{'code':'local','val':metamongo},
                           'dbpedia':{'code':'local','val':metardf}}

                final_results = self.process_single_item_mapping(results)
        else:
            final_results = self.call_external_sources(request)

        #combine mappings between sources
        return final_results

    #this function is used to call external sources apis via their wrapper classes
    #it also makes calls to do the matchings between them
    #and then returns the results (if direct match agreement between sources, it assumes its right, and adds data locally)
    def call_external_sources(self,request):
        print("calling external")
        fieldname = request.keys()[0]
        search_rules = self.config['searchrules']
        external_sources = self.config['external_sources']
        results = {}
        for i in external_sources.keys():
            results[i] = {'code':200, 'val':""}
            if i == "goodreads":
                if fieldname == "title":
                    g = GoodReads(request[fieldname],"title")
                    ct = external_sources[i]["confidence_threshold"]
                    g.set_threshold(ct)
                    rs = g.GET()
                    if g.get_status() == 200:
                        #success
                        if search_rules["goodreads_select_most_rated_as_best"] == True:
                            results[i]['val'] = g.select_book_with_most_ratings()
                        else:
                            #default as a combination of confidence, direct hit with most ratings, just most ratings
                            #each returns a book and a max score ie, {"book":" ", "score": }
                            confidence = g.select_book_with_most_confidence()
                            directhit = g.select_book_with_direct_hit_and_then_most_ratings()
                            mostratings = g.select_book_with_most_ratings()

                            if confidence["score"] != 0:
                                print("confidence score high enough so use it")
                                results[i]['val'] = confidence["book"]
                            elif directhit["score"] >= ct:
                                print("direct hit has high enough confidence")
                                print(directhit["book"])
                                results[i]['val'] = directhit["book"]
                            else:
                                print("neither confidence or direct is high enough, so take most ratings book which has score: ")
                                print(str(mostratings["score"]))
                                results[i]['val'] = mostratings["book"]

                    else:
                        #error
                        results[i]['code'] = g.get_status()

                elif fieldname == "author":
                    #future possible addition search goodreads by author to get books
                    g = GoodReads(request[fieldname],fieldname)
                else:
                    return "Goodreads search by "+fieldname+"not supported"

            elif i == "dbpedia":
                print("now trying dbpedia!!")
                try:
                    attributes = self.config['external_sources']['dbpedia']['searchby'][request.keys()[0]]
                    data = request[ request.keys()[0] ]
                    dbpedia = DBpedia('metadata')
                    print(attributes)
                    print(data)
                    result = dbpedia.searchby(attributes, data)
                    books = result.getBooks()
                    print(books)
                    results[i]['val'] = books
                except:
                    results[i]['code'] = '-1'
            else:
                print("ERROR "+i+" currently not supported by system")

        #now process matchings between sources
        match = self.process_matchings(results)

        if match:
            print("found match")
            goodreads_res = dict()
            dbpedia_res = dict()
            for res in match:
                for ext in external_sources.keys():
                    if ext == 'goodreads':
                        print("goodreads match: ")
                        print(results['goodreads'])

                        attr = (self.config['external_sources']['goodreads']['matching']['title']).split('.')

                        print(results['goodreads']['val'])
                        if results['goodreads']['val'][ attr[0] ][ attr[1] ] == res:
                            goodreads_res[res] = results['goodreads']['val']

                    elif ext == 'dbpedia':
                        dbpedia_match = self.config['external_sources']['dbpedia']['matching']['title']
                        print("looking at dbpedia matches: ")
                        for book in results['dbpedia']['val']:
                            print '/' + book[dbpedia_match]['value'].replace(' ', '_') + ' ' + book['uri']['value']
                            ending = re.match('.*/' + book[dbpedia_match]['value'].replace(' ', '_'), book['uri']['value'])
                            if book[dbpedia_match]['value'] == res and ending:
                                print 'Book accepted!'
                                dbpedia_res[res] = book

            ids = []

            #create a local instance of what we found and agreed upon
            for res in dbpedia_res:
                # We can iterate over dbpedia_res or goodreads_res
                # cause both should have the same length
                # an attribute from dbpedia matching goodreads
                # is the same than an attribute from goodreads
                # matching dbpedia

                print("Create Mysql Doc and Triple")
                print(str(dbpedia_res))
                title = dbpedia_res[res]['title']['value']
                id = self.bck.insert_book(title).id

                triples = dbpedia.constructTriples(dbpedia_res[res]['uri']['value'], str(id))
                print(triples)
                self.qa.insertTriples(triples)

                print("Create Mongo doc")
                print goodreads_res[res]
                mid = self.m.insert(goodreads_res[res])
                self.m.update_book_sqlid(mid, id)
                ids.append(id)
            return ids
        else:
            print("no match between sources")
            #final_results = {'dbpedia':{'code':'local','val':''}}
            final_results = {'dbpedia':{"val":""},"goodreads":{"val":""}}
            final_results['goodreads']['val'] = results['goodreads']['val']
            final_results['dbpedia']['val'] = results['dbpedia']['val']
            return final_results
            #return 'KO'


    def strip_matching_attribute(self, attr):
	    return attr.strip()

    #this is used for matching and could stand to be made more generalizable (by reading from config)
    # , but if we want to add new sources we would need to define their matching here as in any case
    def process_matchings(self, data):
        match = []
        books = dict()
        for external in data:
            if external == 'goodreads':
                books['goodreads'] = dict()
                attr = (self.config['external_sources']['goodreads']['matching']['title']).split('.')
                print(data['goodreads'])
                if len(data['goodreads']['val']) == 0:
                    books['goodreads'][0] = []
                else:
                    books['goodreads'][0] = self.strip_matching_attribute(data['goodreads']['val'][ attr[0] ][ attr[1] ])
                    #print("multiple matches is a bad idea from this source, since dbpedia takes precedence")

                print('Goodreads says book ' + str(books['goodreads']))

            elif external == 'dbpedia':
                books['dbpedia'] = dict()
                attr = self.config['external_sources']['dbpedia']['matching']['title']
                pos = 0
                for book in data['dbpedia']['val']:
                    books['dbpedia'][pos] = self.strip_matching_attribute(book[attr]['value'])
                    print 'DBpedia says book ' + books['dbpedia'][pos]
                    pos = pos + 1

        for a in books:
            for b in books:
                if a != b:
                    for c in books[a]:
                        for d in books[b]:
                            if books[a][c] == books[b][d] and \
                                    not books[a][c] in match:
                                match.append(books[a][c])
        print(match)
        return match

    #helper function to check if two authors are the same
    def confirm_author(self,a,author):
        if a in author or author in a:
            print("direct confirm")
            confirm = 1
        elif "," in a:
            ps = a.split(",")
            if ps[0] in author or ps[1] in author:
                print("confirmed splitting candiate on ,")
                confirm = 1
        elif "," in author:
            ps = author.split(",")
            if ps[0] in a or ps[1] in a:
                print("confirmed splitting dbpedia author on ,")
                confirm = 1
        elif " " in a :
            ps = a.split(" ")[-1]
            if ps in author:
                print("confirmed lastname")
                confirm = 1
        elif " " in author :
            ps = author.split(" ")[-1]
            if ps in a:
                print("confirmed lastname 2")
                confirm = 1
        else:
            confirm = 0
        return confirm


    #if a user selects from a book from a list of external books we returned in regards to their search
    #, we assume they are choosing the book they wanted and then create a local version so we can do ratings on it
    def create_book_from_confirmed(self,uri,title,author):
        print("Create confirmed Mysql Doc and Triple")
        id = self.bck.insert_book(title).id

        dbpedia = DBpedia('metadata')
        triples = dbpedia.constructTriples(uri, str(id))
        print(triples)
        self.qa.insertTriples(triples)


        print("Now try to find a goodreads object with title")
        gr = GoodReads(title,"title")
        gr.GET()
        c1 = gr.select_book_with_most_confidence()
        if c1['score'] != 0:
            candidate = c1['book']
            print("Found candidate with high confidence.  Need to check author")   #TOMORROW JUST CHECK AND FINISH THIS EARLU!  <-- right here!
            print(candidate)
            print("author: "+author)
            if 'author' in candidate['best_book']:
                a = candidate['best_book']['author']['name']
                print("candidate a: "+a)
            else:
                a = candidate[0]['best_book'][0]['author']

            confirm = self.confirm_author(a,author)
            if confirm == 1:
                print("insert into mongodb")
                print(candidate)
                mid = self.m.insert(candidate)
                print("updating "+str(mid)+ " with sql "+str(id))
                self.m.update_book_sqlid(mid,id)
        else:
            directhit = gr.select_book_with_direct_hit_and_then_most_ratings()
            if directhit['score'] != 0:
                candidate = directhit['book']
                print("Found candidate with directhit/most ratings.  Need to check author")
                print(candidate)
                print(author)
                if 'author' in candidate['best_book']:
                    a = candidate['best_book']['author']['name']
                else:
                    a = candidate['best_book'][0]['author']

                confirm = self.confirm_author(a,author)

                if confirm == 1:
                    print("insert into mongodb")
                    print(candidate)
                    mid = self.m.insert(candidate)
                    print("updating "+str(mid)+ " with sql "+str(id))
                    self.m.update_book_sqlid(mid,id)

        if confirm == 0:
            print("Didn't find anything in goodreads with title so to bad.  need to eventually take care of")
            #mostratings = g.select_book_with_most_ratings()
            return "<html><body>Error:  Couldn't find equivalent in Goodreads.  </body></html>"

        return self.singlebook(id)


    #this function was just used for verification of initial data gathering from goodreads
    #as compared to initial tsv file of books given (which were all put in the mysql db)
    def check_mongo(self):
        zs = self.m.find_books_with_zero_sqlid()
        print(zs.count())
        for z in zs:
            t = z['best_book'][0]['title'][0]
            print(str(z['_id']) + "\t" + t + "\t" )
            sq = self.bck.get_book_by_title(t)
            if sq != None:
                print("\t"+sq.title+"\t"+str(sq.id))
                #look if someone else already has this id
                ms = self.m.find_books_by_sqlid(sq.id)
                if ms == None:
                    print("!! None found so update Mongo to Sql "+ str(sq.id))
                    self.m.update_book_sqlid(z['_id'],sq.id)
                else:
                    print("Found book: "+str(ms['_id'])+"\t"+ms["best_book"][0]["title"][0]+" with that sqlid so delete since its a duplicate!!")
                    self.m.delete_by_mongoid(z['_id'])
            else:
                rs = self.bck.get_book_with_title_like(t)
                if rs != None and rs != []:
                    print(rs)
                    print("Found "+str(len(rs))+" possibilities: "+rs[0].title)
                    if len(rs) == 1:
                        sid = rs[0].id
                        ms = self.m.find_books_by_sqlid(sid)
                        if ms == None:
                            print("!@#! None found so update Mongo to Sql "+ str(sid))
                            self.m.update_book_sqlid(z['_id'],sid)
                        else:
                            print("Found book: "+str(ms['_id'])+"\t"+ms["best_book"][0]["title"][0]+" with that sql so delete it since its a duplicate")
                            self.m.delete_by_mongoid(z['_id'])
                else:
                    print("Now trying to split things on ( !")
                    ps = t.split("(")
                    if len(ps) > 1:
                        print("trying with: "+ps[0])
                        rs = self.bck.get_book_with_title_like(ps[0].strip())
                        if rs != None and rs != []:
                            print(rs)
                            print("Found "+str(len(rs))+" possibilities: "+rs[0].title)
                            if len(rs) == 1:
                                sid = rs[0].id
                                ms = self.m.find_books_by_sqlid(sid)
                                if ms == None:
                                    print("!@#! None found so update Mongo to Sql "+ str(sid))
                                    self.m.update_book_sqlid(z['_id'],sid)
                                else:
                                    print("Found book: "+str(ms['_id'])+"\t"+ms["best_book"][0]["title"][0]+" with that sql HMMM")
                                    #self.m.delete_by_mongoid(z['_id'])
                    else:
                        print("Now trying to split things on : !")
                        ps = t.split(":")
                        if len(ps) > 1:
                            print("trying with: "+ps[0])
                            rs = self.bck.get_book_with_title_like(ps[0].strip())
                            if rs != None and rs != []:
                                print(rs)
                                print("Found "+str(len(rs))+" possibilities: "+rs[0].title)
                                if len(rs) == 1:
                                    sid = rs[0].id
                                    ms = self.m.find_books_by_sqlid(sid)
                                    if ms == None:
                                        print("!@#! None found so update Mongo to Sql "+ str(sid))
                                        self.m.update_book_sqlid(z['_id'],sid)
                                    else:
                                        print("Found book: "+str(ms['_id'])+"\t"+ms["best_book"][0]["title"][0]+" with that sql HMMM")
                                        #self.m.delete_by_mongoid(z['_id'])
                        else:
                            print("Now trying to split things on & !")
                            ps = t.split("&")
                            if len(ps) > 1:
                                print("trying with: "+ps[0])
                                rs = self.bck.get_book_with_title_like(ps[0].strip())
                                if rs != None and rs != []:
                                    print(rs)
                                    print("Found "+str(len(rs))+" possibilities: "+rs[0].title)
                                    if len(rs) == 1:
                                        sid = rs[0].id
                                        ms = self.m.find_books_by_sqlid(sid)
                                        if ms == None:
                                            print("!@#! None found so update Mongo to Sql "+ str(sid))
                                            self.m.update_book_sqlid(z['_id'],sid)
                                        else:
                                            print("Found book: "+str(ms['_id'])+"\t"+ms["best_book"][0]["title"][0]+" with that sql HMMM")
                                            #self.m.delete_by_mongoid(z['_id'])
                            else:
                                print("Try spliting on space")
                                ps = t.split(" ")
                                if len(ps) > 1:
                                    if len(ps) < 5: s = 1
                                    if len(ps) > 5: s = 3
                                    term = " ".join(ps[0:len(ps)-s])
                                    print("trying with: "+term)
                                    rs = self.bck.get_book_with_title_like(term)
                                    if rs != None and rs != []:
                                        print(rs)
                                        print("\tFound "+str(len(rs))+" possibilities: "+rs[0].title)
                                        if len(rs) == 1:
                                            sid = rs[0].id
                                            ms = self.m.find_books_by_sqlid(sid)
                                            if ms == None:
                                                print("\t!@#! None found so update Mongo to Sql "+ str(sid))
                                                self.m.update_book_sqlid(z['_id'],sid)
                                            else:
                                                print("\tFound book: "+str(ms['_id'])+"\t"+ms["best_book"][0]["title"][0]+" with that sql HMMM")
                                                #self.m.delete_by_mongoid(z['_id'])



    ##this was used for verification of initial data load from goodreads/mongodb
    def helper(self,sqone,maoneid,startingi,attempt):
        if startingi > 4800:
            ma = MongoAdmin()
            print("------------try i - "+str(startingi)+" ---- attempt "+str(attempt))
            gr = GoodReads(sqone,"title",0)
            gr.load_json_from_file(startingi)
            res = gr.select_book_with_direct_hit_and_then_most_ratings_from_file()
            #overwrite what prior one was
            if res != 0 and res != {}:
                print(res)
                print("------------!!!!!WITH DIFFERENT LOOKUP FOUND: "+res["best_book"][0]["title"][0]+" so updating mongo with its information")
                ma.db.books.update({"_id":maoneid},{"$set": res})
                #print("didn't update")
            else:
                print("------------NOTHING FOUND AGAIN IN ORIGINAL JSON QUERY LOOKUP!!!")
                if attempt < 20:
                    self.helper(sqone,maoneid,startingi-1,attempt+1)


    ##this was only used for verification of initial data load from goodreads/mongodb
    def check_mongo_vs_sql(self):
        #this will check and update mongodb if out of whack
        ma = MongoAdmin()
        bn = Backend()
        #print(ma.db.books.find_one({"sqlid":1},{'best_book.title':1}))
        total = ma.db.books.count()
        sqlids = bn.book_table.query.order_by(Book.id.desc()).first().id
        correct = 0
        counter = 0
        for i in range(1, sqlids):
            sqone = bn.book_table.query.filter_by(id=i).first()
            if sqone != None:
                counter = counter + 1
                sqone = sqone.title
                if type(sqone) != None:
                    #print("looking at sql: "+sqone+" , and id: "+str(i))
                    maone = ma.db.books.find_one({"sqlid":i})
                    if maone != None:
                        if "best_book" in maone:
                            maoneid = maone["_id"]
                            maoneconfirmed = False
                            #if "confirmed" in maone:
                            #    maoneconfirmed = True

                            filetype = 1
                            if len(maone["best_book"]) == 1:
                                maone = maone["best_book"][0]["title"][0]
                            else:
                                maone = maone["best_book"]["title"]
                                filetype = 2

                            if sqone.lower().strip(" ").replace(":","").replace(",","") in maone.lower().strip(" ").replace(":","").replace(",",""):
                                print("CORRECT: for sql id: "+str(i)+", mongo= " +maone+ ", and sql= "+sqone )
                                correct = correct + 1
                            elif maoneconfirmed == True:
                                print("CONFIRMED THAT: "+maone+" is book for sql query "+sqone)
                                correct = correct + 1
                            else:
                                print("ERROR: for sql id: "+str(i)+", mongo= " +maone+ ", and sql= "+sqone )
                                print("----- looking for correct in mongodb")
                                res = ma.db.books.find({'best_book.title':{'$regex': "^"+sqone, '$options':'i'}})
                                if res.count() == 0:
                                    print("------------ERROR: didn't find anything close in mongodb so check json file "+str(counter)+".json")

                                    #use different technique for lookup.  0 is to skip initialization
                                    gr = GoodReads(sqone,"title",0)
                                    gr.load_json_from_file(counter)
                                    res = gr.select_book_with_direct_hit_and_then_most_ratings_from_file()

                                    #overwrite what prior one was
                                    if res != 0 and res != {}:
                                        print(res)
                                        #res["sqlid"] = i
                                        correct = correct + 1
                                        res["confirmed"]= 1
                                        print("------------WITH DIRECT HIT LOOKUP FOUND: "+res["best_book"][0]["title"][0]+" so updating mongo with its information")
                                        #ma.db.books.update({"_id":maoneid},{"$set": res})
                                    else:
                                        print("------------NOTHING FOUND IN ORIGINAL JSON QUERY LOOKUP WITH DIRECT HIT SO TRY HIGH CONFIDENCE MEASURE!")
                                        #self.helper(sqone,maoneid,i-340,1)
                                        res = gr.select_book_with_most_confidence_from_file()
                                        if res != 0 and res !={}:
                                            print("------------WITH HIGH CONFIDENCE FOUND: "+res["best_book"][0]["title"][0]+" so updating mongo with its information")
                                            #ma.db.books.update({"_id":maoneid},{"$set":res})
                                        else:
                                            print("------------NOTHING FOUND IN ORIGINAL JSON QUERY LOOKUP WITH HIGH CONFIDENCE SO GO WITH JUST BEST RATED!!!")

                                            res = gr.select_book_with_most_ratings_from_file()
                                            if res != 0 and res !={}:
                                                res["confirmed"]= 1
                                                print("------------------------WITH BEST RATED LOOKUP FOUND: "+res["best_book"][0]["title"][0]+" so updating mongo with its information")
                                                #ma.db.books.update({"_id":maoneid},{"$set": res})
                                                correct = correct + 1
                                            else:
                                                print("------------------------NOTHING FOUND WITH BEST RATED")

                                else:
                                    print("------------FOUND IN MONGODB")
                                    if res.count() == 1:
                                        first = res.next()
                                        title = first["best_book"][0]["title"][0]
                                        print("----------------Found one result: "+title+" and will give it sqlid: "+str(i))
                                        ma.db.books.update({"_id":first["_id"]},{"$set": {'sqlid':i}})
                                        print("----------------ALSO SET other mongo record to sqlid 0")
                                        ma.db.books.update({"_id":maoneid},{"$set": {"sqlid":0}})
                                        correct = correct + 1
                                    else:
                                        tots = res.count()
                                        print("----------------!*@ Found "+str(tots)+"thus do this by hand!!")
                                        print("----------------JUST USE THE ONE THAT IS CLOSEST TO ID: "+str(i))
                                        bestc = -1
                                        currid = 0
                                        for c in range(0,tots):
                                            cobj = res.next()
                                            if bestc == -1:
                                                bestc = cobj
                                                currid = cobj["sqlid"]
                                            elif abs(cobj["sqlid"] - i) < abs(currid - i):
                                                bestc = cobj
                                                currid = cobj["sqlid"]

                                        print("-----------------Best Found is "+str(currid)+": "+bestc["best_book"][0]["title"][0])
                                        ma.db.books.update({"_id":bestc["_id"]},{"$set" : {"sqlid":i}})
                                        print("----------------ALSO SET other mongo record to sqlid 0")
                                        ma.db.books.update({"_id":maoneid},{"$set": {"sqlid":0}})
                                        correct = correct + 1

        print(str(correct) + " correct out of "+ str(total))
        print("counter "+str(counter))




#THESE ARE ALL THE SERVICES EXPOSED TO THE FRONTEND

#home page
@app.route('/', methods=['GET'])
def index():
    m = Middleware()
    return m.home()


#user profile page
@app.route('/profile', methods=['GET'])
def userprofile():
    m = Middleware()
    return m.profile()

#single book page
@app.route('/book/<bid>')
def bookview(bid):
    m = Middleware()
    return m.singlebook(bid)

#confirm an external book service
@app.route('/confirm',methods=['POST'])
def confirmbook():
    print("in confirmbook")
    title = FlaskRequest.form['title']
    uri = FlaskRequest.form['uri']
    author = FlaskRequest.form['author']
    print("doing confirmbook with title: "+title+", uri: "+uri+", and author: "+author)
    m = Middleware()
    return m.create_book_from_confirmed(uri,title,author)

#rate a book service
@app.route('/rate/', methods=['GET'])
def rate_service():
    rating = FlaskRequest.args.get('rating')
    userid = FlaskRequest.args.get('userid')
    bookid = FlaskRequest.args.get('bookid')
    ratingstype = FlaskRequest.args.get('ratingstype')
    m = Middleware()
    if ratingstype == "decimal":
        print("insert decimal rating")
        m.bck.insert_rating(userid,bookid,rating)
    else:
        print("insert binary rating")
        m.bck.insert_binary_rating(userid,bookid,rating)

    return m.singlebook(bookid)

#search local first service (if not found calls external sources)
@app.route('/searchby/', methods=['GET'])
def search_service():
    ts = FlaskRequest.args.get('titlesearch')
    request = {'title':ts}
    m = Middleware()
    userid = FlaskRequest.cookies.get('userid')
    ratingstype = FlaskRequest.cookies.get('ratingstype')

    #first check if book already in DB, and if so show it and recommendations
    already_in_db = m.bck.get_book_with_title_like(ts)
    if already_in_db != None and already_in_db != []:
        if len(already_in_db) == 1:
            print("possibly already in db")
            print(already_in_db[0])
            bid = already_in_db[0].id
            allow_external_search = ts
            return m.singlebook(bid,allow_external_search)
        else:
            print("found multiple books..")
            return m.show_list_of_books(already_in_db,ts)
    else:
        print("found nothing locally")
        #if not, do search
        #content = m.search_request(request)
        content = m.call_external_sources(request)
        if type(content) == dict:
            # No matchings found, so we  just return a big JSON file with all the possible books  found in DBpedia
            print("no matching so return possible responses to front")
            print(content)
            return m.show_for_confirmation(content,ts)

        elif type(content) == list:
            # Matchings found, so we just return a list of the new books SQL IDs
            if len(content) == 1:
                print("One match found")
                # Only one match found, then great, we just have that book
                print content[0]
                return m.singlebook(content[0])
            else:
                print("More than one match found")
                print id
                #because we implemented good reads source to only return high quality (ie, less false positives) we won't reach here

        elif type(content) == str and content == 'KO':    # Oops... Something went wrong
            print("Error")
            print content
        else:
            print "nothing"

        #this is only reached in case of an error
        return "<html><body>error</body></html>"

#
@app.route('/searchexternal/', methods=['GET'])
def external_search_service():
    ts = FlaskRequest.args.get('titlesearch')
    m = Middleware()
    userid = FlaskRequest.cookies.get('userid')
    ratingstype = FlaskRequest.cookies.get('ratingstype')
    request = {'title':ts}
    print("Trying external call with title: "+ts)
    content = m.call_external_sources(request)
    if type(content) == dict:
        # No matchings found, so we  just return a big JSON file with all the possible books  found in DBpedia
        print("no matching so return possible responses to front")
        print(content)
        return m.show_for_confirmation(content,ts)

    elif type(content) == list:
        # Matchings found, so we just return a list of the new books SQL IDs
        if len(content) == 1:
            print("One match found")
            # Only one match found, then great, we just have that book
            print content[0]
            return m.singlebook(content[0])
        else:
            print("More than one match found")
            # More than one matching? The user needs to choose among the matches
            print id
    elif type(content) == str and content == 'KO':    # Oops... Something went wrong
        print("Error")
        print content
    else:
        print "nothing"

    #this is only reached in case of an error
    return "<html><body>error</body></html>"



if __name__ == '__main__':

    app.debug = True
    app.run()
    #print(m.get_config())

    #m = Middleware()
    #m.check_mongo()

    #s = m.bck.get_book_with_title_like("The Quillan Games")
    #print(len(s))
    #print(s[0].title)

    #mmm = m.m.find_books_by_sqlid(5243)
    #print(mmm)
    #print(mmm["_id"])
    #print(mmm["best_book"][0]["title"][0])

    #m.search_request({"title":"Unicorn Variations"})
    #m.check_mongo_vs_sql()
    #ma = MongoAdmin()
    '''
    ma1 = ma.db.books.find_one({"sqlid":274})
    ma2 = ma.db.books.find_one({"sqlid":275})
    print(ma1["best_book"])
    print(len(ma1["best_book"]))
    print(ma2["best_book"])
    print(len(ma2["best_book"]))
    '''

    #bn = Backend()
    #print(dir(bn.book_table))
    #print(dir(bn.book_table.query))
    #print(bn.book_table.query.count())
    #print(bn.book_table.query(bn.book_table.id, func.max(bn.book_table.id)))
    #print(bn.book_table.query.order_by(bn.book_table.id).first().id)
    #print(bn.book_table.query.order_by(Book.id.desc()).first().id)
    #print(bn.book_table.query.filter_by(id=253).first())

    #ma = MongoAdmin()
    #print(ma.db.books.find_one({"sqlid":2336}))

    #http://altons.github.io/python/2013/01/21/gentle-introduction-to-mongodb-using-pymongo/

    #res = ma.db.books.find({'best_book.title':{'$regex': "The Rebel Angels", '$options':'i'}})
    #print(dir(res))
    #print(res.count())

    #res2 = ma.db.books.find({'best_book.title':{'$regex':"William Cooper's Town: Power and Persuasion on the Frontier of the Early American Republic", '$options':'i'}})
    #print(dir(res2))
    #print(res2.count())
    #first = res2.next()
    #print(first["best_book"][0]['title'])
    #print(first["_id"])
    #print(first["sqlid"])
    #ma.db.books.update({"_id":first["_id"]},{"$set": {'sqlid':17}})

    #st = "Politika"
    #res3 = ma.db.books.find({'best_book.title':{'$regex':st,'$options':'i'}})
    #print(res3.count())


    #st = "King Lear"
    #res3 = ma.db.books.find({'best_book.title':{'$regex':st,'$options':'i'}})
    #print(res3.count())
    #for i in range(0,res3.count()):
        #print(res3.next()["sqlid"])

    '''
    maone = ma.db.books.find_one({'sqlid':11})
    print(maone.__class__)
    print(maone)
    maoneid = maone["_id"]
    print(maoneid)
    gr = GoodReads("The Rebel Angels","title")
    gr.load_json_from_file(11)
    res = gr.select_book_with_direct_hit_and_then_most_ratings()
    print(res["best_book"][0]["title"][0])
    #res["_id"] = maoneid
    #res.a = maoneid
    #print(res)
    #ma.db.books.update({"_id":maoneid},{"$set": res})
    '''
