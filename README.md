# Science Fiction and Hyperchaos: <br>Digital Humanities as Extro-Criticism

by Graham Joncas & Nora Li

Written for <a href="http://dadh2018.dila.edu.tw/?lang=en">DADH 2018</a>: 9th International Conference of Digital Archives & Digital Humanities<br>&nbsp;



<b>Abstract (100 words)</b>:<br>

This paper compares Hugo and Nebula award-winning short stories using text mining and logistic regression. Science fiction is known for its radical singularity: each text is an ‘event’ in the philosophical sense, creating a universe unto itself. In this light, unlike traditional criticism, quantitative methods generalize in the absence of unifying conventions or topoi. Parallel to Meillassoux's concept of extro-science fiction, digital humanities acts as ‘extro-criticism’ within fields of radical contingency (‘hyperchaos’). This asemic forensics not only traces 114 stories’ lexical detritus to each award’s institutional schemata, but presages xenographic re-mappings for conventional literary notions of ‘code’, ‘genre’, and ‘text’.



## How to Replicate
<ol>

<li>Unzip the data files into your working directory</li>

<li>Open sci-fi.R in RStudio (for R v3.5.1 or later)</li>

<li>Click ‘Source’ to extract the story metadata</li>

<li>To create charts, use the timeseries() function</li>

<li>For alternate regressions, redefine the scifi dataframe</li>

</ol>



## Comments

<ul>

<li>My first DH paper. The philosophy part is exciting, the results fail to live up to the hype.</li>

<li>The writing was a bit rushed, so I still want to experiment a bit (e.g. SVMs, new variables)</li>

<li>My code heavily uses the global assignment operator <<-, which feels like bad practice.</li>

<li>NL thought of comparing the Hugo & Nebula awards, and collected the data.</li>

<li>GJ thought of the theoretical framework, and wrote the code and prose.
</li>
</ul>

&nbsp;<br>
This paper and code is licensed under <a href="https://creativecommons.org/licenses/by/4.0">CC BY 4.0</a><br>
In short: it's fine to use the data, just don't plagiarize our paper.
