# generate_test_sets
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gridExtra) #For plotting lots of things at once

# source('hex_setup.R')
load('./dat/hexsetup.rdata', verbose = T)


# ##################################################
# Generate a bunch of test sequences without caching

set.seed(1988)

depth<-5
N<-100
basic_targets_d5<-matrix(NA, nrow=61, ncol=N)
basic_targets_d5_solutions<-list()


for (i in 1:100)
{
  # Basic functionality is to chain these together, calculating a loss and keeping a history
  state<-empty_state %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0)
  
  # Generate a random sequence (in which every step actually does something)
  d<-1
  a_states<-matrix(NA, nrow=nrow(state), ncol=depth)#vector(mode = "list", length = depth)
  actions<-rep(NA, length = depth)
  while (d<(depth+1))
  {
    action<-sample(length(f), 1)
    tmp_state<-f[[action]](state)
    
    loop_check<-F
    if (d>1)
    {
      for (lc in 1:(d-1))
      {
        if(all(tmp_state$active==a_states[,lc]))
        {
          loop_check<-T
          cat(i, d, lc, 'loop detected\n')
        }
      }
    }
    
    
    if (loop_check==F)
    {
      state<-tmp_state
      a_states[,d]<-state$active
      actions[d]<-names(f)[action]
      d<-d+1
    }
  }

  basic_targets_d5[,i]<-state$active
  basic_targets_d5_solutions[[i]]<-actions
}

save(file='./dat/basic_targets_d5.rdata', basic_targets_d5, basic_targets_d5_solutions)

# Mega-plot time
p<-fb<-list()
for (i in 1:N)
{
  fb[[i]]<-board_polygons
  fb[[i]]$state<-factor(rep(basic_targets_d5[,i], each = 6))
  p[[i]]<-ggplot(fb[[i]], aes(x_pos, y_pos)) + 
    geom_polygon(aes(group = id, fill=state), colour = 'black') +
    scale_fill_manual(values = c('white','yellow')) +
    ggtitle(paste0(basic_targets_d5_solutions[[i]], collapse=' '))
  
}

p_save<-grid.arrange(grobs = p, nrow = round(sqrt(N)))
ggsave(p_save, filename='./plot/targets.pdf', width = 50, height = 40, limitsize = F)


# Now for some recursive targets
set.seed(1988)
depth<-5
N<-10
recursive_targets_d5<-list()
recursive_targets_d5_solutions<-list()


for (cache in 1:N)
{
  recursive_targets_d5[[cache]]<-matrix(NA, nrow=61, ncol=10)
  recursive_targets_d5_solutions[[cache]]<-list()
  
  for (example in 1:N)
  {
    # Basic functionality is to chain these together, calculating a loss and keeping a history
    state<-empty_state %>% select(-x_pos, -y_pos) %>% mutate(target = 0, active=0)
    
    # Generate a random sequence (in which every step actually does something)
    d<-1
    a_states<-matrix(NA, nrow=nrow(state), ncol=depth)#vector(mode = "list", length = depth)
    actions<-list()
    
    
    while (d<(depth+1))
    {
      if (runif(1)>0.5)
      {
        action<-names(f)[sample(length(f), 1)]
        tmp_state<-f[[action]](state)
        cat('cache', cache, ' example', example, 'primitive\n')
      } else {
        action<-'Cache'#basic_targets_d5_solutions[[cache]]
        tmp_state<-apply_cache(state, basic_targets_d5_solutions[[cache]])
        cat('cache', cache, ' example', example, 'library\n')
      }
      
      
      # Checks the state actually changed (reduces redundancies a lot)
      loop_check<-F
      if (d>1)
      {
        for (lc in 1:(d-1))
        {
          if(all(tmp_state$active==a_states[,lc]))
          {
            loop_check<-T
            cat(i, d, lc, 'loop detected\n')
          }
        }
      }
      
      
      if (loop_check==F)
      {
        state<-tmp_state
        a_states[,d]<-state$active
        actions[[d]]<-action
        d<-d+1
      }
    }
    recursive_targets_d5[[cache]][,example]<-state$active
    recursive_targets_d5_solutions[[cache]][[example]]<-actions
  }
  
}



# Mega-plot time

for (cache in 1:N)
{
  p<-fb<-list()
  for (example in 1:N)
  {
    fb[[example]]<-board_polygons
    fb[[example]]$state<-factor(rep(recursive_targets_d5[[cache]][,example], each = 6))
    p[[example]]<-ggplot(fb[[example]], aes(x_pos, y_pos)) + 
      geom_polygon(aes(group = id, fill=state), colour = 'black') +
      scale_fill_manual(values = c('white','yellow')) +
      ggtitle(paste0(recursive_targets_d5_solutions[[example]][[cache]], collapse=' '))
  }
  p_save<-grid.arrange(grobs = p, nrow = 2,
                       top = paste0('Cache sequence: ', paste0(basic_targets_d5_solutions[[cache]], collapse = ' ')))
  ggsave(p_save, filename=paste0('./plot/recursive_targets', cache, '.pdf'), width = 30, height = 10, limitsize = F)
}


save(file='./dat/recursive_targets_d5.rdata', basic_targets_d5, basic_targets_d5_solutions)