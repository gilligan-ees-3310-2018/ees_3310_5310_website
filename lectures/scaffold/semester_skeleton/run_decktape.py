import os
import sys
import subprocess
import re
import platform

system = platform.system().lower()
if (system == 'windows'):
    decktape = 'decktape.bat'
elif (system == 'linux'):
    decktape = 'decktape'

args = ['reveal', '-s', '1920x1080']

lecture_template = "EES_2110_5110_Class_%02d.pdf"
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
    cmd = [decktape,] + args + [infile_name, outfile_name]
    subprocess.call(cmd)
    os.chdir(home)

def runall_decktape(path):
  files = os.listdir(path)
  for f in files:
    ff = os.path.join(path,f)
    if os.path.isdir(ff) and f.lower().startswith('lecture'):
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
                runall_decktape('Lectures')
        else:
            for a in sys.argv[1:]:
                if (os.path.isdir(a) and a.lower().startswith('lecture') and os.path.exists(os.path.join(a, 'index.html'))):
                    run_decktape(a)
                elif (os.path.isfile(a)):
                    run_decktape(a)
                elif a.isdigit():
                    a = os.path.join('Lectures', 'Lecture_%02d' % int(a), 'index.html')
                    if os.path.exists(a):
                        run_decktape(a)

if __name__ == '__main__':
    main()
