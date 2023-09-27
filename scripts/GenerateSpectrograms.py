# Sept 2023
# A script to generate spectrograms representing 12-second segments of audio in
# the frequency range [0, 4000 Hz]. Splits up the spectrogram generation across
# multiple folders. Progress bar generally behaves.

import math, os, subprocess, sys, time, wave
import multiprocessing as mp
from multiprocessing import JoinableQueue, Process, Queue

# Adjust this if necessary
sox_path = r"C:\Program Files (x86)\sox-14-4-2\sox.exe"

# Returns a sorted list of all .wav files under topdir.
def findFiles(topdir, ext):
    files_found = []
    for root, dirs, files in os.walk(topdir):
        for file in files:
            if file.split('.')[-1] == ext.replace('.', ''):
                files_found.append(os.path.join(root, file))
    return sorted(files_found)

# Returns the length of a given wav file in hours or seconds.
def getWavLength(wav_path, mode='h'):
    try:
        with wave.open(wav_path) as w:
            nframes, framerate = w.getnframes(), w.getframerate()
        wav_length_s = float(nframes) / framerate
    except:
        wav_length_s = 0
    wav_length_h = wav_length_s / 3600.
    if mode == 'h':
        return wav_length_h
    else:
        return wav_length_s

# Returns the size of a given file in gigabytes (or technically gibibytes).
def getFileSizeGB(file_path):
    gb_conversion = float(1024**-3)
    size_gb = os.path.getsize(file_path) * gb_conversion
    return size_gb

# Returns a list of SoX commands that will create spectrograms for every 12 s
# of audio in the wav file in question.
def makeSoxCmds(wav_path, sox_path, output_dir):
    wav_name = os.path.basename(wav_path)
    wav_length = getWavLength(wav_path, 's')
    n_segments = int(wav_length / 12) + 1
    n_digits = max(len(str(n_segments)), 3)
    sox_cmds = []
    for i in range(1, n_segments+1):
        offs = 12 * (i - 1)
        if offs + 12 > wav_length:
            dur = wav_length - offs
            if dur < 8:
                continue
        else:
            dur = 12
        png_name = wav_name.replace(wav_name[-4:], "_part_{0}.png".format(str(i).zfill(n_digits)))
        png_path = os.path.join(output_dir, png_name)
        sox_cmd = 'sox "{0}" -V1 -n trim {1} {2} remix 1 rate 8k spectrogram -x 1000 -y 257 -z 90 -m -r -o {3}'.format(wav_path, offs, dur, png_path)
        sox_cmds.append(sox_cmd)
    return sox_cmds

# Return a list of tuples giving the path to wav files and the path to the folder
# where spectrograms from each will be generated.
def makeSpectroDirList(wav_list, image_dir, n_chunks):
    chunk_size = int(len(wav_list) / n_chunks) + 1
    n_digits = int(math.log10(n_chunks)) + 1
    wav_chunks = [int(i / chunk_size) + 1 for i in range(len(wav_list))]
    dst_dirs = [os.path.join(image_dir, "part_{0}".format(str(j).zfill(n_digits))) for j in wav_chunks]
    wav_key = list(zip(wav_list, dst_dirs))
    return wav_key

# Makes a nice formatted text progress bar
def makeProgBar(done, total, width=30):
    prop_done = done / total
    n_fill = int(prop_done * width)
    pct_done = "{0:.1f}".format(prop_done * 100)
    sep_char = '=' if done == total else '>'
    prog_bar = "[{0}{1}{2}] {3}/{4} ({5}%)".format("="*n_fill, sep_char, "."*(width-n_fill), done, total, pct_done)
    return prog_bar

class WaveWorker(Process):
    def __init__(self, in_queue, done_queue, sox_path, output_dir):
        Process.__init__(self)
        self.in_queue = in_queue
        self.done_queue = done_queue
        self.sox_path = sox_path
        self.output_dir = output_dir
    
    def run(self):
        while True:
            wav_path, spectro_dir = self.in_queue.get()
            sox_cmds = makeSoxCmds(wav_path, self.sox_path, spectro_dir)
            for i in sox_cmds:
                os.system(i)
            self.in_queue.task_done()
            self.done_queue.put(wav_path)

class ProgBarWorker(Process):
    def __init__(self, done_queue, total_size):
        Process.__init__(self)
        self.done_queue = done_queue
        self.total_size = total_size
        self.done = 0

    def run(self):
        print(makeProgBar(0, self.total_size, 30), end='\r')
        while True:
            n_done = self.done_queue.qsize()
            if n_done < self.total_size:
                if n_done > self.done:
                    progbar = makeProgBar(n_done, self.total_size, 30)
                    print(progbar, end='\r')
                    self.done = n_done
                else:
                    print('', end='\r')
                time.sleep(2)
            else:
                break

def main():
    n_cores = mp.cpu_count()

    target_dir= sys.argv[1]
    try:
        output_dir = sys.argv[2]
        image_dir = os.path.join(output_dir, os.path.basename(target_dir), "images")
    except:
        output_dir = target_dir
        image_dir = os.path.join(output_dir, "Temp", "images")
    
    wav_paths = findFiles(target_dir, "wav")
    n_wav_files = len(wav_paths)
    
    wav_lengths = [getWavLength(x) for x in wav_paths]
    wav_sizes = [getFileSizeGB(y) for y in wav_paths]
    total_dur, total_gb = sum(wav_lengths), sum(wav_sizes)
    
    print("\nFound {0} wav files.\nTotal duration: {1:.1f} h\nTotal size: {2:.1f} GB".format(n_wav_files, total_dur, total_gb))
    
    if n_wav_files == 0:
        print("\nExiting.")
        exit()

    n_images = total_dur * 300
    n_chunks = int(n_images / 50000) + min(n_images % 50000, 1)

    todo = makeSpectroDirList(wav_paths, image_dir, n_chunks)
    spectro_dirs = list(set([k[1] for k in todo]))
    for l in spectro_dirs:
        if not os.path.exists(l):
            os.makedirs(l)

    wav_queue, done_queue = JoinableQueue(), Queue()
    for i in todo:
        wav_queue.put(i)

    print("\nGenerating spectrograms using {0} cores starting at {1}...".format(n_cores, time.strftime("%H:%M:%S")))

    for j in range(n_cores):
        worker = WaveWorker(wav_queue, done_queue, sox_path, image_dir)
        worker.daemon = True
        worker.start()

    prog_worker = ProgBarWorker(done_queue, n_wav_files)
    prog_worker.daemon = True
    prog_worker.start()

    wav_queue.join()

    end_prog_bar = makeProgBar(n_wav_files, n_wav_files)
    print(end_prog_bar)
    
    pngs = findFiles(image_dir, "png")
    
    print("\nFinished at {0}.\n{1} spectrograms generated.\n".format(time.strftime("%H:%M:%S"), len(pngs)))

if __name__ == "__main__":
    main()