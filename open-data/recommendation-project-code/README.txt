This is a project for the Open Data course at the Universitat Politecnica de Catalunya (BarcelonaTech)
in the Spring of 2014.  

To install the book recommender system,
you must have installed mysql, 4store, mongodb and python.
The data files for the initial mysql, 4store and mongodb are looked in the data/ folder 
and should be imported in before starting 
( it will be necessary to make a db called "recdb" for mysql  and a db called "recdb" for mongodb)
Then after the data has been loaded, make sure the path to metadata/config.json is set correctly in middleware.py

After confirming that, just run "python middleware.py"  to start the system.
There is currently no "requirements.txt" to help install external python libraries so its possible this 
may have to be done by hand (via easy_install, pip, etc )

once the program is running, open a brower and go to http://127.0.0.1:5000/  

for updates see: www.github.com/diegoolano 
