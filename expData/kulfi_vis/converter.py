with open('/home/yang/git/kulfi/expData/abilene/EdgeCongestionVsIterations.dat') as f:
    content=f.readlines()
fileName=''
base_dir='/home/yang/WebstormProjects/kulfi_vis/'
curf=-1
files_repo=open('/home/yang/WebstormProjects/kulfi_vis/files.json','w')
files_repo.write('[\n')
firstName=True
for i in range(1,len(content)):
    if not (':' in content[i]):
        if (curf!=-1):
            curf.write('\n]')
            curf.close()
        fileName=base_dir+content[i].replace(' ','').replace('\t','_')[:-2]+'.txt'
        if (len(content[i])>3):
            #rint(fileName) 
            if (firstName):
                firstName=False
            else:
                files_repo.write(',\n')
            files_repo.write('{'+'"name":"'+content[i].replace(' ','').replace('\t','_')[:-2]+'.txt'+'"}')
        else: 
            break
        curf=open(fileName,'w')        
        curf.write('[\n')
        newline=True
    else:
        #curf.write(content[i])
        if (newline):
            newline=False
        else:
            curf.write(',\n')
        curf.write('{')
        b=content[i].split(':')
        c=b[0].replace('\t','').replace('(','').replace(')','').replace(' ','').split(',')
        curf.write('"source": "'+c[0]+'", "target": "'+c[1]+
                   '","capacity":"1000", "util":"'+str(float(b[1])*100+10)+'"')
        curf.write('}')
files_repo.write('\n]')
files_repo.close()
