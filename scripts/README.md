* The jupyter notebook contains the Monte-Carlo co-occurrence algorithm that we developed, and generates heatmaps presented in figure 5 in the main text and supplementary figures A3 and A4 in the appendix.
* call_rate.R runs a test for the difference in call rates (i.e note emission rates) between contexts in _N. humayuni_.
* gen_transition_probability_matrices_(FOMC).R models note emission as a first-order Markov chain, generates estimated transition matrices, and runs statistical tests (specifically, [Kullback's homogeneity test](https://www.jstor.org/stable/1266291?seq=1#metadata_info_tab_contents)) on these matrices.These transition probabilities are presented in figure 4(b) in the main text.
* get_fig4_circle_sizes.R generates circle diameters for the circles presented in figure 4(b) in the main text.
* make_note_proportion_plots.R creates the boxplots presented in figure 3(a) and 3(c) of the main text, and performs the stats summarized in table 2 in the main text.
* nycti_number_of_DNs_appended.R creates the plot presented in figure 3(b) of the main text.
* get_shannon_entropy.R gets the Shannon entropy of the vocal sequences, and creates figure 4(a) of the main text.
* nycti_locationwise_PCA.R generates a PCA of the vocalizations of _N. humayuni_, and creates the plot presented in figure A1 in the appendix, as well as the PCA loadings presented in table A1.
