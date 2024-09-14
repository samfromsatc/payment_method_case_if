import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.font_manager as fm
from plotnine import ggplot, aes, geom_line, geom_bar, theme, element_blank, element_line, element_text

colors = mcolors.TABLEAU_COLORS



os.chdir('C:/Repos/payment_method_case_if') 

imp = pd.read_excel("PaymentCasefromFinland_data.xlsx")

#sanity check 'grouped values': har jag förstått datan rätt. en rad per kombination av variabelutfall och 'customers' anger hur många med den kombinationen?
sc_grouped = imp.drop(columns = ['customers']).drop_duplicates()

print(len(sc_grouped) - len(imp)) # finns bara distinkta kombinationer av värden

grunddata = (imp
             .assign(Användare = lambda x: np.where(x['FeatureInvestigated'] == 1, 'Användare', 'Övriga'),
                     Rating = lambda x: x['Rating'].str.title())
             )

def custom_theme(): 
    return (theme(
        panel_background=element_blank(),
        axis_ticks=element_blank(),
        panel_grid_major_y=element_line(color="grey"),
        axis_title=element_text(size=14, family='Helvetica')
    ))

#Slide 1: Vilka åldrar använder metoden?
by_age_df = (
    grunddata.groupby(['AgeClass', 'FeatureInvestigated'])
    .agg(Antal = ('customers', 'sum'))
    .reset_index()
    .assign(Andel = lambda x: x.groupby('AgeClass')['Antal'].transform(lambda y:100*y/y.sum()))
    .query('FeatureInvestigated == 1')
    .assign(AgeClass = lambda x: pd.Categorical(x['AgeClass'],
                                                          categories=['Missing', 'Group1', 'Group2', 'Group3', 'Group4'],
                                                          ordered=True))
    )

plt_age = (ggplot(by_age_df) +
           aes(x='AgeClass', y='Andel') +
           geom_bar(stat = 'identity', fill = colors['tab:blue']) +
           custom_theme() +
           theme(axis_title_x = element_blank()))


#slide 2 påverkar rating benägenheten?
by_rating_df = (
    grunddata.groupby(['Rating', 'FeatureInvestigated'])
    .agg(Antal = ('customers', 'sum'))
    .reset_index()
    .assign(Andel = lambda x: x.groupby('Rating')['Antal'].transform(lambda y:100*y/y.sum()))
    .query('FeatureInvestigated == 1')
    .assign(Rating = lambda x:pd.Categorical(x['Rating'],
                                             categories=['Missing', 'OK', 'Good', 'Star']))
)

plt_rating = (ggplot(by_rating_df) +
              aes(x='Rating', y='Andel') + 
              geom_bar(stat = 'identity', fill = colors['tab:purple']) +
              custom_theme()

)

print(plt_rating)

# slide 3: cube rating bland användare och övriga

cube_rating_df = (
    grunddata.query('cube_level != "?"'))

print(cube_rating_df)

"""available_fonts = fm.findSystemFonts(fontpaths=None, fontext='ttf')
print(available_fonts)"""