import sqlite3
import numpy

con = sqlite3.connect('/home/prc33/Dropbox/dovetail/results.db')
con.row_factory = sqlite3.Row

def get_realtime(benchmark, version, threads=None, n=None):
    cur = con.cursor()
    where = ""
    
    if threads is not None:
        where = where + "threads=" + str(threads) + " AND "
    
    if n is not None:
        where = where + "n=" + str(n) + " AND "
    
    where = where + 'benchmark="' + benchmark + '" AND version="' + version + '"'
    
    cur.execute("SELECT threads, n, AVG(t_real) AS `time`, COUNT(id) AS `count` FROM timings WHERE " + where + " GROUP BY threads, n ORDER BY threads")
    
    rows = cur.fetchall()
    
    if len(rows) == 0:
      return None
    elif len(rows) == 1:
      return rows[0]['time']
    else:
      return (numpy.array([x['threads'] for x in rows]), numpy.array([x['time'] for x in rows]))
