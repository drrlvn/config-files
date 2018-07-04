function cmk -w ssh -d 'Close SSH master connection'
    ssh -O exit
end
