import os
import sys
import subprocess
import re
import platform

system = platform.system().lower()
if (system == 'windows'):
    decktape = ['old_decktape.bat',]
elif (system == 'linux'):
    # decktape = 'decktape'
    decktape = ['~/decktape/bin/phantomjs','~/decktape/decktape.js']
    decktape = list(map(os.path.expanduser, decktape))

# subprocess.call(decktape + ['-h',], shell=True)

# args = ['reveal', '-s', '1920x1080']
# args = ['reveal', '-s', '1280x1024']
args = ['reveal', '-s', '1440x900']

lecture_template = "EES_3310_5310_Class_%02d_Slides.pdf"
classnum_expr = re.compile('.*_(?P<class_num>[0-9]+)$')


def run_decktape(path):
    if os.path.isfile(path):
        path, infile_name = os.path.split(path)
    else:
        infile_name = 'index.html'
    if not os.path.exists(os.path.join(path, infile_name)):
        return
    if infile_name == 'index.html':
        tail = os.path.split(path)[1]
        m = classnum_expr.match(tail)
        if m is None:
            return
        classnum = int(m.group('class_num'))
        outfile_name = lecture_template % classnum
    else:
        outfile_name = infile_name.replace('.html', '.pdf')
    home = os.getcwd()
    os.chdir(path)
    print("Changing directory from ", home, " to ", path)
    cmd = decktape + args + [infile_name, outfile_name]
    print(cmd)
    subprocess.call(cmd)
    os.chdir(home)

def runall_decktape(path):
  files = os.listdir(path)
  for f in files:
    ff = os.path.join(path,f)
    if os.path.isdir(ff) and f.lower().startswith('class_'):
      run_decktape(ff)
      runall_decktape(ff)

def main():
    if len(sys.argv) > 1:
        if sys.argv[1] in ('-a', '--all'):
            if len(sys.argv) > 2:
                for a in sys.argv[2]:
                    if (os.path.isdir(a)):
                        runall_decktape(a)
            else:
                runall_decktape('Slides')
        else:
            for a in sys.argv[1:]:
                if (os.path.isdir(a) and a.lower().startswith('slides') and os.path.exists(os.path.join(a, 'index.html'))):
                    run_decktape(a)
                elif (os.path.isfile(a)):
                    run_decktape(a)
                elif a.isdigit():
                    a = os.path.join('Slides', 'Class_%02d' % int(a), 'index.html')
                    print("checking ", a)
                    if os.path.exists(a):
                        run_decktape(a)

if __name__ == '__main__':
    main()
