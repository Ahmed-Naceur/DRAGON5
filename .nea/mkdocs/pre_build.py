from pathlib import Path
import shutil
import subprocess

here = Path(__file__).parent

DOCSDIR=here.parents[1] / "docs"
CSS="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"
STY= here / "IGNrapportPandocCompatible.sty"

# copy all existing documentation to `docs` folder for clean working space
DOCSDIR.mkdir(exist_ok=True)
shutil.copytree(here.parents[1] / 'doc', DOCSDIR)

# add customizations to docs
shutil.copytree(here/'customization', DOCSDIR, dirs_exist_ok=True)

# cleanup of previous builds
for image in DOCSDIR.glob('IGE*/images/*.eps'):
    image.unlink()
for image in DOCSDIR.glob('IGE*/images/*.png'):
    image.unlink()
for image in DOCSDIR.glob('images/*.eps'):
    image.unlink()
for image in DOCSDIR.glob('images/*.png'):
    image.unlink()
for report in DOCSDIR.glob('IGE*.md'):
    report.unlink()


setup_scripts = {"IGE335":"""ln -s ../../Dragon/data/tmacro_proc/TCM01.c2m TCM01.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM02.c2m TCM02.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM03.c2m TCM03.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM04.c2m TCM04.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM05.c2m TCM05.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM06.c2m TCM06.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM07.c2m TCM07.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM08.c2m TCM08.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM09.c2m TCM09.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM10.c2m TCM10.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM11.c2m TCM11.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM12.c2m TCM12.x2m
ln -s ../../Dragon/data/tmacro_proc/TCM13.c2m TCM13.x2m
#
ln -s ../../Dragon/data/twlup_proc/TCWU01.c2m TCWU01.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU02.c2m TCWU02.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU03.c2m TCWU03.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU04.c2m TCWU04.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU05.c2m TCWU05.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU06.c2m TCWU06.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU07.c2m TCWU07.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU08.c2m TCWU08.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU09.c2m TCWU09.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU10.c2m TCWU10.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU11.c2m TCWU11.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU12.c2m TCWU12.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU13.c2m TCWU13.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU14.c2m TCWU14.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU15.c2m TCWU15.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU17.c2m TCWU17.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU31.c2m TCWU31.x2m
ln -s ../../Dragon/data/twlup_proc/TCWU05Lib.c2m .
ln -s ../../Dragon/data/twlup_proc/TCWU17Lib.c2m .
#
ln -s ../../Dragon/data/tmatxs7a_proc/TCXA01.c2m TCXA01.x2m
#
ln -s ../../Dragon/data/assertS.c2m .
ln -s ../../Dragon/data/assertV.c2m .
#
ln -s ../../readme readme
#
chmod 644 TCM* TCWU* TCXA* assert*.c2m
""",
"IGE344":"""
ln -s ../../Donjon/data/Example1.x2m Example1.x2m
ln -s ../../Donjon/data/Example2.x2m Example2.x2m
ln -s ../../Donjon/data/proc/Pburn.c2m Pburn.c2m
ln -s ../../Donjon/data/proc/Pdevc.c2m Pdevc.c2m
ln -s ../../Donjon/data/proc/Pgeom.c2m Pgeom.c2m
ln -s ../../Donjon/data/proc/Pfmap.c2m Pfmap.c2m
"""}


clean_tmp_latex="""
rm *.aux
rm *.dvi
rm *.idx
rm *.lof
rm *.log
rm *.log
rm *.out
rm *.toc
rm *.4ct
rm *.4tc
rm *.xref
rm *.lot"""

pandocify_figures=r"sed -i 's/\\epsffile{\([^}]*\)}/\\includegraphics{\1}/g' *.tex"



for report in ["IGE332", "IGE335", "IGE344", "IGE369"]: # Skipping "IGE351" for now due to latex issues
    # overwrite IGNRapport style file with Pandoc compatible version
    shutil.copy(STY, DOCSDIR / report / "IGNrapport.sty")

    # clean temporary latex files
    subprocess.run(clean_tmp_latex, capture_output=True, shell=True, cwd=f"{DOCSDIR / report}")
    subprocess.run(pandocify_figures, capture_output=True, shell=True, cwd=f"{DOCSDIR / report}")

    # subproject-specific setup
    if setup_script:=setup_scripts.get(report, None):
        subprocess.run(setup_script, capture_output=True, shell=True, cwd=f"{DOCSDIR/ report}")
    
    # latex to markdown conversion
    command = ["pandoc", "--standalone", "-t", "markdown", "--mathml", "--extract-media=images", "--self-contained", "-f", "latex", f"{report}.tex", f"--lua-filter={here/'latex-to-mkdocs-refs.lua'}", "-o", f"{DOCSDIR}/{report}.md"]
    subprocess.run(command, check=True, cwd=f"{DOCSDIR/report}")
    
    # move image folder to correct relative path with respect to output markdown
    shutil.copytree(DOCSDIR/report/'images', DOCSDIR/'images', dirs_exist_ok=True)

    # convert eps images to web-compatible (mkdocs compatible) png
    for eps_image in (DOCSDIR / 'images').glob('*.eps'):
        # enable imagemagick to handle eps file via cli
        # sudo sed -i 's/<policy domain="coder" rights="none" pattern="PS" \/>/<policy domain="coder" rights="read" pattern="PS" \/>/g' /etc/ImageMagick-6/policy.xml
        # increase memory limit to process large files
        # sudo sed -i 's/<policy domain="resource" name="memory" value="256MiB"\/>/<policy domain="resource" name="memory" value="4GiB"\/>/g' /etc/ImageMagick-6/policy.xml
        command = f"convert -colorspace RGB -density 300 -quality 90 {eps_image} {eps_image.with_suffix('.png')}"
        subprocess.run(command, check=True, cwd=f"{DOCSDIR}", shell=True)

    # replace references to .eps files
    command = rf"sed -i 's/\.eps)/\.png)/g' {report}.md"
    subprocess.run(command, check=True, cwd=DOCSDIR, shell=True)

    
        

