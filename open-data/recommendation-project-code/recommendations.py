#Recommendations.py
# NB: we choose a user based recommender over an item based recommender although the item based one is likely to change less often. Assumption: less user so will run more quickly this way.
from math import sqrt

class Recommendations:
    def __init__(self):
        self.help = ""

    ## DISTANCES : no need to update every time!
    # similarity distance : pearson score (correlation: similarity of "tastes"(: ordering) more than of grades)
    # Returns the Pearson correlation coefficient for p1 and p2
    def sim_pearson(self,prefs,p1,p2):
        # Get the list of mutually rated items
        si={}

        if p1 in prefs:
            for item in prefs[p1]:
                if item in prefs[p2]:
                    si[item]=1

        # Find the number of elements
        n=len(si)

        # if they are no ratings in common, return 0
        if n==0:
            #print("RETURN zero")
            return 0

        '''
        print("----------------for p1: "+str(p1)+" and p2: "+str(p2))
        print(si)
        # Add up all the preferences
        print("book1")
        for it in si:
            print(prefs[p1][it])

        print("book2")
        for it in si:
            print(prefs[p2][it])
        '''

        sum1=sum([prefs[p1][it] for it in si])
        sum2=sum([prefs[p2][it] for it in si])

        # Sum up the squares
        sum1Sq=sum([pow(prefs[p1][it],2) for it in si])
        sum2Sq=sum([pow(prefs[p2][it],2) for it in si])

        # Sum up the products
        pSum=sum([prefs[p1][it] * prefs[p2][it] for it in si])

        #print("sum1: "+str(sum1) + ", sum2: "+str(sum2)+", n: "+str(n)+", sum1Sq: "+str(sum1Sq)+", sum2Sq: "+str(sum2Sq)+", pSum: "+str(pSum))
        # Calculate Pearson score
        num= pSum - float(sum1*sum2/n)

        den=sqrt((sum1Sq-pow(sum1,2)/n)*(sum2Sq-pow(sum2,2)/n))

        #print("num:" +str(num)+", den: "+str(den))
        if den==0:
            #print("return zero")
            return 0

        r=float(num)/den

        #print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!NOT ZERO r= "+str(r))
        return r * 10000000

    # Euclidian distance: for -1/0/1 scores

    # Returns a distance-based similarity score for person1 and person2
    def sim_distance(self,prefs,person1,person2):
        # Get the list of shared_items
        si={}
        if person1 not in prefs:
            return 'KO'

        for item in prefs[person1]:
            if item in prefs[person2]:
                si[item]=1
        # if they have no ratings in common, return 0
        if len(si)==0: return 0
        # Add up the squares of all the differences
        sum_of_squares=sum([pow(prefs[person1][item]-prefs[person2][item],2)
                            for item in prefs[person1] if item in prefs[person2]])
        return 1/(1+sum_of_squares)

    ## / DISTANCES

    ## USERS SIMILARITIES
    #Similarities Users: Ranking neighbours
    # Returns the best matches for person from the prefs dictionary.
    # Number of results and similarity function are optional params.
    def topMatches(self,prefs,person,similarity,n=4):
        if similarity == 'sim_pearson':
            scores=[(self.sim_pearson(prefs,person,other),other) for other in prefs if other!=person]
        else:
            scores=[(self.sim_distance(prefs,person,other),other) for other in prefs if other!=person]
        # Sort the list so the highest scores appear at the top
        #print(scores)
	    if scores == 'KO':
		    return []

        scores.sort( )
        scores.reverse( )
        return scores[0:n]

    # Get recommendation of Books from the similar users
    # Gets recommendations for a person by using a weighted average
    # of every other user's rankings
    def getRecommendations(self,prefs,person,similarity):
        totals={}
        simSums={}
        numS = {}
        for other in prefs:
            # don't compare me to myself
            if other==person: continue
            if similarity=='sim_pearson':
                sim=self.sim_pearson(prefs,person,other)
            else:
                sim=self.sim_distance(prefs,person,other)

            # ignore scores of zero or lower
            if sim<=0: continue
            for item in prefs[other]:
                # only score movies I haven't seen yet
                if item not in prefs[person] or prefs[person][item]==0:
                    # Similarity * Score
                    totals.setdefault(item,0)
                    totals[item]+=prefs[other][item]*sim
                    # Sum of similarities
                    simSums.setdefault(item,0)
                    simSums[item]+=sim
                    numS.setdefault(item,0)
                    numS[item]+=1

        # Create the normalized list
        #print("TOTAL!!")
        #print(totals.items())
        rankings=[(total/simSums[item],numS[item],item) for item,total in totals.items( )]
        #print("***********************RANKINGS***********************")
        #print(rankings)
        # Return the sorted list
        rankings.sort( )
        #print("***********************RANKINGS SORTED***********************")
        #print(rankings)

        rankings.reverse( )
        #print("***********************RANKINGS REVERSED***********************")
        #print(rankings)


        return rankings

    ## /USER SIMILARITIES

    ##SIMILAR BOOKS
    # Finding similar Books without regard for Users:

    #transpose:
    def transformPrefs(self,prefs):
        result={}
        for person in prefs:
            for item in prefs[person]:
                result.setdefault(item,{})
                # Flip item and person
                result[item][person]=prefs[person][item]
        return result

    # similarities: # top.Matches
    ##/ SIMILAR BOOKS

    #Cold cold start
    def best_books(self,prefs,userbased=0):
        # this expects a book json object with each book having the users who've rated it along with the ratings
        # this returns a list of the books with the highest ratings..
        # list is of form (rating,itemid)

        if userbased == 1:
            #if user centric as opposed to book centric, change it
            tprefs = self.transformPrefs(prefs)
        else:
            tprefs = prefs

        scores = []
        max = 0
        for item in tprefs:
            sc = 0
            numb = 0
            for persone in tprefs[item]:
                sc = sc + tprefs[item][persone]
                numb = numb + 1
            if max<numb : max=numb
            scores.append((float(sc)/numb,numb,item))

        # Sort the list so the highest scores appear at the top
        scores = [(ra*(5+nb/max)/6 ,nb,item) for ra, nb, item in scores]
        scores.sort( )
        scores.reverse()
        return scores

##MAIN
if __name__ == '__main__':
    print("hello")
    # A dictionary of movie critics and their ratings of a small
    # set of movies
    critics={'Lisa Rose': {'Lady in the Water': 2.5, 'Snakes on a Plane': 3.5,
                           'Just My Luck': 3.0, 'Superman Returns': 3.5, 'You, Me and Dupree': 2.5,
                           'The Night Listener': 3.0},
             'Gene Seymour': {'Lady in the Water': 3.0, 'Snakes on a Plane': 3.5,
                              'Just My Luck': 1.5, 'Superman Returns': 5.0, 'The Night Listener': 3.0,
                              'You, Me and Dupree': 3.5},
             'Michael Phillips': {'Lady in the Water': 2.5, 'Snakes on a Plane': 3.0,
                                  'Superman Returns': 3.5, 'The Night Listener': 4.0},
             'Claudia Puig': {'Snakes on a Plane': 3.5, 'Just My Luck': 3.0,
                              'The Night Listener': 4.5, 'Superman Returns': 4.0,
                              'You, Me and Dupree': 2.5},
             'Mick LaSalle': {'Lady in the Water': 3.0, 'Snakes on a Plane': 4.0,
                              'Just My Luck': 2.0, 'Superman Returns': 3.0, 'The Night Listener': 3.0,
                              'You, Me and Dupree': 2.0},
             'Jack Matthews': {'Lady in the Water': 3.0, 'Snakes on a Plane': 4.0,
                               'The Night Listener': 3.0, 'Superman Returns': 5.0, 'You, Me and Dupree': 3.5},
             'Toby': {'Snakes on a Plane':4.5,'You, Me and Dupree':1.0,'Superman Returns':4.0}}

    rec = Recommendations()
#print(rec.sim_distance(critics,'Lisa Rose','Gene Seymour'))
# test similarities pearson users:
#print(rec.sim_pearson(critics,'Lisa Rose','Gene Seymour'))
# test similar list:
#print(rec.topMatches(critics,'Toby',similarity='sim_pearson',n=3))

#Book recommendation (given a user), pearson dist:
#print(rec.getRecommendations(critics,'Toby',similarity='sim_pearson'))
#book recommendation: euclidian score:
#print(rec.getRecommendations(critics,'Toby',similarity='sim_distance'))

#Get similar books (without users)
#movies=rec.transformPrefs(critics)
#print(rec.topMatches(movies,'Superman Returns',similarity='sim_pearson'))

#cold start
#print (rec.best_books(critics))
	
