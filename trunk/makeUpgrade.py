"""
Builds the manifest for a Myro upgrade package.
"""

import sys, os, datetime, posixpath

#upgrade_against = sys.argv[1] # 2.0.2
upgrade_name    = sys.argv[1] # 2.5.0

#ctime = os.path.getctime("../html/myro/myro-%s.zip" % upgrade_against)
#stime = datetime.date.fromtimestamp(ctime)
#print """svn diff -r {%s}:HEAD | grep " myro/" | cut -c 8- """ % stime
print("""find myro | grep -v /osc/ | grep "\.py$" """)
#pipe = os.popen("""svn diff -r {%s}:HEAD | grep " myro/" | cut -c 8- """ % stime)
pipe = os.popen("""find myro | grep -v /osc/ | grep "\.py$" """)

manifest = {}
for line in pipe:
    line = line.strip()
    prefix, filename = line.split("myro/", 1)
    #filename = filename.replace(",v", "")
    path_filename = filename
    if "/" in filename:
        path, filename = filename.rsplit("/", 1)
    count = 1
    newfilename = filename
    while newfilename in manifest:
        newfilename = filename + ("-%d" % count)
        count += 1
    manifest[newfilename] = (filename, path_filename)
pipe.close()

os.system("rm -f myro-upgrade-%s.zip" % (upgrade_name, ))
os.system("rm -rf myro-upgrade-%s MANIFEST" % (upgrade_name, ))
os.system("mkdir myro-upgrade-%s" % upgrade_name)
manifest_files = manifest.keys()
manifest_files.sort()
mfp = open("myro-upgrade-%s/MANIFEST" % upgrade_name, "w")
first = 1
completed = []
file_set = list(set(manifest_files))
file_set.sort()
file_set.reverse()
for newfilename in file_set:
    filename, path_filename = manifest[newfilename]
    # if file exists:
    if (posixpath.exists("myro/%s" % path_filename) and 
        os.path.isfile("myro/%s" % path_filename)):
        if "myro/%s" % path_filename in completed: continue
        completed.append("myro/%s" % path_filename)
        if not first:
            print >> mfp, ""
        print >> mfp, "%s: %%(PYTHONSITEDIR)s/myro/%s" % (newfilename,path_filename),
        os.system("cp myro/%s myro-upgrade-%s/%s" %
                  (path_filename, upgrade_name, newfilename))
        first = 0
mfp.close()
os.system("cd myro-upgrade-%s; zip ../myro-upgrade-%s.zip *" %
          (upgrade_name, upgrade_name))
os.system("rm -rf myro-upgrade-%s/ MANIFEST" % (upgrade_name, ))
