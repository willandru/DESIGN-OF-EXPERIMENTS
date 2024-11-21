import ffmpeg

# Input and output file paths
input_file = "u2_final.avi"
output_file = "output_video.mp4"

# Convert AVI to MP4
ffmpeg.input(input_file).output(output_file, vcodec="libx264", acodec="aac").run()
