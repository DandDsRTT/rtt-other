optimizeGeneratorTuningMap["[⟨31 49 72 87 107]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨22 35 51 62 76]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨12 19 28 34 42]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨15 24 35 42 52]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨41 65 95 115 142]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨27 43 63 76 94]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨14 22 32 39 48]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨19 30 44 53 66]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨9 14 21 25 31]⟩", "minimax-S"]
optimizeGeneratorTuningMap["[⟨19 30 44 53 65]⟩", "minimax-S"]

randomTandU[] := Module[{d, grade, ma, t, u},
  d = RandomInteger[{1, 5}];
  grade = RandomInteger[{1, d}];
  ma = RandomInteger[{-9, 9}, {grade, d}];
  
  t = If[RandomInteger[] == 1, {ma, "col"}, {ma, "row"}];
  u = matrixToMultivector[t];
  
  {t, u}
];

mas = {};
Do[
  d = RandomInteger[{2,6}];
  r = RandomInteger[{1, d-1}];
  ma = RandomInteger[{-9, 9}, {r, d}];
  ma = canonicalForm[ma];
  AppendTo[mas, ma]
      10
];

{(5.310),(16.226),(7.233),(4.548),(38.762),(12.504),(52.325),(17.407),(5.404),(8.226),(5.042),(4.109),(14.641),(8.056),(12.126),(8.448),(7.641),(5.042),(7.892),(17.638),(5.581),(5.042),(171.245),(5.530),(8.762),(25.617),(4.548),(5.883),(8.696),(36.380),(19.048),(4.582),(5.744),(23.111),(10.346),(10.079),(4.670),(7.181),(4.255),(17.902),(4.381),(5.714),(5.823),(7.999),(4.582),(109.472),(10.262),(8.636),(4.563),(5.021),(10.805),(4.395),(12.246),(6.486),(85.447),(4.580),(8.518),(6.700),(12.921),(194.975),(10.341),(4.332),(4.124),(23.547),(3.884),(13.317),(4.509),(9.912),(5.062),(5.712),(66.121),(8.273),(21.822),(42.933),(8.338),(34.433),(15.810),(63.360),(11.872),(10.804),(4.495),(8.696),(12.246),(23.111),(6.975),(7.550),(99.806),(6.975),(7.268),(4.182),(7.494),(14.293),(4.706),(5.240),(13.040),(4.745),(5.714),(22.642),(70.515),(4.722),(8.572),(25.013),(46.282),(9.754),(12.258),(8.518),(4.316),(75.466),(6.154),(5.214),(4.028),(13.794),(4.426),(11.872),(7.639),(4.651),(66.121),(8.335),(10.170),(5.407),(30.723),(21.089),(21.414),(27.906),(4.651),(12.246),(4.494),(7.367),(14.116),(47.907),(8.814),(4.780),(13.032),(6.153),(5.966),(6.032),(4.040),(399.713),(7.999),(594.552),(7.639),(6.518),(5.108),(5.556),(7.738),(19.052),(41.468),(5.003),(4.096),(5.357),(8.335),(8.451),(21.821),(7.018),(109.472),(19.379),(4.529),(27.242),(35.320),(5.743),(11.316),(7.549),(7.999),(20.664),(10.269),(7.851),(4.286),(41.468),(41.403),(4.081),(19.640),(4.615),(4.124),(43.041),(5.285),(11.436),(4.444),(1061.074),(25.024),(5.823),(4.444),(5.502),(9.912),(5.529),(14.648),(11.544),(4.957),(6.217),(5.196),(7.017),(4.332),(5.796),(5.530),(47.907),(54.578),(5.240),(11.111),(16.866),(7.181),(15.200),(13.660),(5.333),(4.285),(7.318),(6.486),(13.660),(8.636),(5.214),(8.273),(12.013),(7.847),(4.109),(23.547),(30.075),(8.895),(13.660),(8.518),(4.580),(8.959),(9.449),(5.530),(10.346),(1130.363),(9.093),(14.837),(92.049),(109.472),(7.948),(6.596),(17.927),(17.638),(4.444),(15.586),(35.258),(9.600),(6.154),(7.267),(4.153),(7.494),(9.517),(11.659),(7.144),(10.346),(66.264),(5.478),(13.479),(5.798),(5.883),(15.997),(7.181),(6.452),(4.510),(5.240),(4.082),(4.563),(8.163),(6.188),(6.557),(5.084),(3.859),(4.780),(8.225),(5.452),(3.973),(306.156),(16.880),(16.226),(4.001),(7.738),(7.495),(14.837),(6.317),(24.036),(3.921),(4.395),(9.156),(5.631),(3.907),(6.776),(6.421),(5.966),(5.108),(13.475),(171.245),(34.433),(7.694),(5.061),(13.486),(8.390),(12.614),(3.960),(9.912),(19.048),(10.517),(149.639),(30.762),(6.000),(8.814),(5.714),(4.707),(44.432),(6.188),(16.414),(11.212),(30.075),(196.810),(4.350),(52.325),(4.332),(399.713),(4.764),(8.338),(9.449),(10.269),(12.495),(308.262),(38.757),(17.124),(7.267),(12.368),(6.345),(4.743),(12.246),(85.447),(33.350),(70.808),(6.421),(70.808),(5.579),(4.580),(4.529),(92.049),(5.152),(8.963),(14.283),(5.196),(30.075),(57.247),(4.444),(24.038),(44.432),(19.640),(4.301),(4.529),(15.586),(11.201),(6.283),(5.851),(50.017),(30.075),(4.494),(3.884),(3.973),(50.026),(7.948),(14.641),(3.907),(4.938),(41.403),(12.118),(13.958),(4.580),(11.316),(43.041),(20.291),(8.636),(19.052),(4.055),(5.766),(9.999),(7.268),(4.209),(4.066),(4.977),(22.631),(5.556),(4.363),(134.249),(4.097),(7.407),(4.899),(27.242),(63.360),(4.939),(20.301),(16.226),(17.902),(57.247),(5.358),(18.765),(8.819),(27.906),(4.563),(13.958),(149.639),(5.266),(5.941),(11.544),(4.548),(5.310),(4.363),(19.640),(4.529),(32.334),(6.596),(7.364),(11.544),(4.899),(5.480),(4.197),(22.203),(9.449),(7.494),(13.191),(5.265),(7.847),(10.437),(6.345),(6.381),(3.884),(12.246),(6.120),(4.529),(5.630),(7.318),(10.913),(7.791),(4.066),(5.851),(9.023),(6.557),(5.529),(5.129),(7.147),(4.411),(6.670),(4.109),(6.557),(4.316),(4.412),(11.876),(4.168),(3.859),(4.615),(6.486),(3.859),(3.936),(4.596),(5.478),(12.118),(14.986),(19.654),(6.670),(7.406),(4.615),(10.173),(4.272),(7.454),(29.265),(8.891),(17.629),(10.262),(5.452),(12.126),(6.059),(8.696),(4.181),(4.124),(38.714),(11.659),(7.099),(79.726),(11.316),(11.544),(4.395),(5.043),(6.034),(4.316),(18.480),(7.694),(26.690),(4.271),(39.863),(4.444),(10.079),(5.021),(4.039),(11.002),(6.776),(18.162),(46.271),(15.819),(14.648),(9.376),(16.880),(5.796),(5.062),(9.912),(52.501),(6.596),(22.642),(10.437),(12.368),(8.959),(10.437),(23.547),(4.316),(44.432),(4.495),(21.093),(6.452),(39.811),(12.368),(5.966),(6.346),(20.291),(52.501),(39.811),(13.792),(12.619),(9.313),(23.547),(16.675),(92.049),(3.907),(11.212),(4.582),(5.129),(4.109),(27.916),(6.486),(100.051),(3.884),(11.438),(6.381),(3.973),(23.547),(5.407),(9.229),(66.264),(6.092),(7.015),(3.920),(6.486),(4.239),(12.368),(4.395),(4.706),(5.357),(13.486),(4.066),(7.893),(52.325),(4.801),(4.839),(5.002),(4.411),(6.668),(7.999),(13.660),(5.581),(10.804),(7.601),(4.444),(3.884),(6.154),(21.822),(134.249),(5.766),(9.377),(5.766),(5.310),(16.676),(6.449),(17.124),(31.591),(13.794),(12.118),(22.203),(8.819),(5.909),(6.702),(25.617),(6.742),(9.600),(5.478),(10.346),(46.271),(14.988),(5.823),(17.139),(6.817),(12.246),(9.229),(5.042),(12.361),(4.743),(4.301),(4.271),(7.367),(5.285),(13.660),(17.118),(4.001),(3.872),(8.819),(4.285),(12.126),(4.096),(4.529),(15.810),(9.519),(4.285),(8.390),(12.915),(4.426),(9.155),(16.880),(4.899),(8.390),(5.128),(85.431),(4.181),(4.818),(29.265),(59.999),(70.515),(6.631),(14.641),(3.973),(4.977),(57.247),(21.414),(134.249),(3.884),(4.818),(5.767),(4.395),(4.381),(4.548),(4.152),(22.644),(7.791),(5.608),(5.285),(17.118),(5.911),(16.676),(15.587),(4.109),(4.316),(10.913),(70.515),(21.093),(6.002),(8.448),(7.550),(35.341),(4.124),(4.209),(85.431),(6.519),(5.690),(7.789),(4.027),(15.990),(4.096),(7.694),(13.317),(12.614),(5.266),(7.550),(6.346),(5.767),(19.654),(3.921),(33.269),(10.005),(6.702),(4.001),(25.585),(10.262),(30.762),(11.111),(26.690),(5.478),(4.395),(3.884),(9.755),(6.596),(6.119),(6.558),(17.139),(7.847),(37.472),(6.700),(5.240),(19.379),(17.407),(5.430),(4.411),(26.091),(399.713),(66.264),(5.214),(4.670),(36.380),(4.137),(8.449),(19.379),(5.529),(13.652),(6.155),(37.472),(11.111),(4.272),(6.631),(32.334),(7.230),(33.279),(9.093),(5.529),(17.902),(15.586),(6.742),(5.084),(5.286),(5.407),(7.364),(5.172),(8.963),(27.916),(5.502),(5.407),(4.097),(7.739),(4.363),(37.472),(7.018),(3.872),(52.501),(41.468),(6.283),(9.449),(5.108),(5.128),(4.957),(21.399),(8.888),(4.285),(21.093),(25.013),(6.154),(134.190),(5.712),(7.948),(4.839),(7.495),(63.278),(5.852),(10.707),(4.818),(5.002),(12.019),(5.851),(8.335),(23.547),(593.318),(5.911),(7.948),(7.454),(3.897),(63.360),(10.170),(7.495),(6.190),(4.272),(35.320),(6.060),(63.278),(14.293),(4.671),(9.018),(27.242),(10.920),(7.738),(6.936),(4.529),(13.788),(11.876),(9.376),(17.638),(4.027),(6.519),(6.155),(4.632),(16.425),(66.264),(9.018),(13.792),(23.547),(6.449),(11.876),(4.706),(7.948),(5.110),(18.459),(21.821),(10.170),(36.380),(399.713),(5.631),(4.818),(5.581),(4.722),(16.676),(5.407),(4.688),(3.948),(6.059),(12.619),(13.195),(10.346),(13.794),(3.896),(8.386),(47.836),(35.320),(5.085),(5.883),(8.057),(4.899),(4.510),(4.722),(399.713),(12.771),(57.388),(19.048),(8.390),(7.847),(4.781),(7.183),(19.379),(12.019),(5.128),(6.556),(7.062),(4.395),(9.375),(11.316),(5.357),(6.094),(12.368),(9.093),(12.368),(30.075),(4.839),(4.239),(6.862),(5.266),(10.443),(4.818),(4.066),(4.494),(4.596),(6.188),(7.015),(9.377),(1130.363),(5.084),(5.883),(43.041),(5.310),(5.407),(5.383),(24.437),(13.660),(13.479),(3.936),(4.494),(5.214),(6.596),(4.670),(10.346),(17.902),(16.866),(14.837),(7.794),(18.468),(13.486),(17.638),(5.152),(4.227),(15.586),(4.096),(5.310),(8.636),(5.968),(3.987),(5.042),(6.092),(32.334),(5.285),(10.622),(50.017),(4.272),(34.433),(14.988),(6.120),(12.118),(5.712),(6.317),(30.762),(4.818),(9.754),(6.817),(27.242),(14.283),(8.106),(17.629),(7.791),(9.229),(44.336),(5.108),(54.578),(7.600),(5.966),(79.726),(7.181),(85.447),(11.212),(7.738),(4.688),(12.368),(4.745),(14.103),(6.556),(7.454),(30.762),(8.636),(18.468),(7.598),(7.999),(5.662),(4.197),(6.217),(6.449),(6.556),(134.190),(8.757),(5.529),(5.581),(9.093),(5.608),(6.817),(8.056),(5.197),(11.117),(8.163),(4.209),(46.296),(10.700),(19.052),(5.452),(75.535),(5.286),(15.361),(4.444),(20.664),(10.809),(8.963),(57.247),(14.116),(92.049),(16.880),(4.706),(11.201),(4.272),(54.578),(3.872),(6.897),(20.301),(8.386),(4.670),(4.209),(5.310),(11.753),(42.933),(3.960),(6.283),(4.818),(4.780),(25.585),(8.757),(5.083),(9.912),(13.788),(9.999),(8.163),(33.350)}
{(5.310),(16.226),(7.233),(4.548),(38.762),(12.504),(52.325),(17.407),(5.404),(8.226),(5.042),(4.109),(14.641),(8.056),(12.126),(8.448),(7.641),(5.042),(7.892),(17.638),(5.581),(5.042),(171.245),(5.530),(8.762),(25.617),(4.548),(5.883),(8.696),(36.380),(19.048),(4.582),(5.744),(23.111),(10.346),(10.079),(4.670),(7.181),(4.255),(17.902),(4.381),(5.714),(5.823),(7.999),(4.582),(109.472),(10.262),(8.636),(4.563),(5.021),(10.805),(4.395),(12.246),(6.486),(85.447),(4.580),(8.518),(6.700),(12.921),(194.975),(10.341),(4.332),(4.124),(23.547),(3.884),(13.317),(4.509),(9.912),(5.062),(5.712),(66.121),(8.273),(21.822),(42.933),(8.338),(34.433),(15.810),(63.360),(11.872),(10.804),(4.495),(8.696),(12.246),(23.111),(6.975),(7.550),(99.806),(6.975),(7.268),(4.182),(7.494),(14.293),(4.706),(5.240),(13.040),(4.745),(5.714),(22.642),(70.515),(4.722),(8.572),(25.013),(46.282),(9.754),(12.258),(8.518),(4.316),(75.466),(6.154),(5.214),(4.028),(13.794),(4.426),(11.872),(7.639),(4.651),(66.121),(8.335),(10.170),(5.407),(30.723),(21.089),(21.414),(27.906),(4.651),(12.246),(4.494),(7.367),(14.116),(47.907),(8.814),(4.780),(13.032),(6.153),(5.966),(6.032),(4.040),(399.713),(7.999),(594.552),(7.639),(6.518),(5.108),(5.556),(7.738),(19.052),(41.468),(5.003),(4.096),(5.357),(8.335),(8.451),(21.821),(7.018),(109.472),(19.379),(4.529),(27.242),(35.320),(5.743),(11.316),(7.549),(7.999),(20.664),(10.269),(7.851),(4.286),(41.468),(41.403),(4.081),(19.640),(4.615),(4.124),(43.041),(5.285),(11.436),(4.444),(1061.074),(25.024),(5.823),(4.444),(5.502),(9.912),(5.529),(14.648),(11.544),(4.957),(6.217),(5.196),(7.017),(4.332),(5.796),(5.530),(47.907),(54.578),(5.240),(11.111),(16.866),(7.181),(15.200),(13.660),(5.333),(4.285),(7.318),(6.486),(13.660),(8.636),(5.214),(8.273),(12.013),(7.847),(4.109),(23.547),(30.075),(8.895),(13.660),(8.518),(4.580),(8.959),(9.449),(5.530),(10.346),(1130.363),(9.093),(14.837),(92.049),(109.472),(7.948),(6.596),(17.927),(17.638),(4.444),(15.586),(35.258),(9.600),(6.154),(7.267),(4.153),(7.494),(9.517),(11.659),(7.144),(10.346),(66.264),(5.478),(13.479),(5.798),(5.883),(15.997),(7.181),(6.452),(4.510),(5.240),(4.082),(4.563),(8.163),(6.188),(6.557),(5.084),(3.859),(4.780),(8.225),(5.452),(3.973),(306.156),(16.880),(16.226),(4.001),(7.738),(7.495),(14.837),(6.317),(24.036),(3.921),(4.395),(9.156),(5.631),(3.907),(6.776),(6.421),(5.966),(5.108),(13.475),(171.245),(34.433),(7.694),(5.061),(13.486),(8.390),(12.614),(3.960),(9.912),(19.048),(10.517),(149.639),(30.762),(6.000),(8.814),(5.714),(4.707),(44.432),(6.188),(16.414),(11.212),(30.075),(196.810),(4.350),(52.325),(4.332),(399.713),(4.764),(8.338),(9.449),(10.269),(12.495),(308.262),(38.757),(17.124),(7.267),(12.368),(6.345),(4.743),(12.246),(85.447),(33.350),(70.808),(6.421),(70.808),(5.579),(4.580),(4.529),(92.049),(5.152),(8.963),(14.283),(5.196),(30.075),(57.247),(4.444),(24.038),(44.432),(19.640),(4.301),(4.529),(15.586),(11.201),(6.283),(5.851),(50.017),(30.075),(4.494),(3.884),(3.973),(50.026),(7.948),(14.641),(3.907),(4.938),(41.403),(12.118),(13.958),(4.580),(11.316),(43.041),(20.291),(8.636),(19.052),(4.055),(5.766),(9.999),(7.268),(4.209),(4.066),(4.977),(22.631),(5.556),(4.363),(134.249),(4.097),(7.407),(4.899),(27.242),(63.360),(4.939),(20.301),(16.226),(17.902),(57.247),(5.358),(18.765),(8.819),(27.906),(4.563),(13.958),(149.639),(5.266),(5.941),(11.544),(4.548),(5.310),(4.363),(19.640),(4.529),(32.334),(6.596),(7.364),(11.544),(4.899),(5.480),(4.197),(22.203),(9.449),(7.494),(13.191),(5.265),(7.847),(10.437),(6.345),(6.381),(3.884),(12.246),(6.120),(4.529),(5.630),(7.318),(10.913),(7.791),(4.066),(5.851),(9.023),(6.557),(5.529),(5.129),(7.147),(4.411),(6.670),(4.109),(6.557),(4.316),(4.412),(11.876),(4.168),(3.859),(4.615),(6.486),(3.859),(3.936),(4.596),(5.478),(12.118),(14.986),(19.654),(6.670),(7.406),(4.615),(10.173),(4.272),(7.454),(29.265),(8.891),(17.629),(10.262),(5.452),(12.126),(6.059),(8.696),(4.181),(4.124),(38.714),(11.659),(7.099),(79.726),(11.316),(11.544),(4.395),(5.043),(6.034),(4.316),(18.480),(7.694),(26.690),(4.271),(39.863),(4.444),(10.079),(5.021),(4.039),(11.002),(6.776),(18.162),(46.271),(15.819),(14.648),(9.376),(16.880),(5.796),(5.062),(9.912),(52.501),(6.596),(22.642),(10.437),(12.368),(8.959),(10.437),(23.547),(4.316),(44.432),(4.495),(21.093),(6.452),(39.811),(12.368),(5.966),(6.346),(20.291),(52.501),(39.811),(13.792),(12.619),(9.313),(23.547),(16.675),(92.049),(3.907),(11.212),(4.582),(5.129),(4.109),(27.916),(6.486),(100.051),(3.884),(11.438),(6.381),(3.973),(23.547),(5.407),(9.229),(66.264),(6.092),(7.015),(3.920),(6.486),(4.239),(12.368),(4.395),(4.706),(5.357),(13.486),(4.066),(7.893),(52.325),(4.801),(4.839),(5.002),(4.411),(6.668),(7.999),(13.660),(5.581),(10.804),(7.601),(4.444),(3.884),(6.154),(21.822),(134.249),(5.766),(9.377),(5.766),(5.310),(16.676),(6.449),(17.124),(31.591),(13.794),(12.118),(22.203),(8.819),(5.909),(6.702),(25.617),(6.742),(9.600),(5.478),(10.346),(46.271),(14.988),(5.823),(17.139),(6.817),(12.246),(9.229),(5.042),(12.361),(4.743),(4.301),(4.271),(7.367),(5.285),(13.660),(17.118),(4.001),(3.872),(8.819),(4.285),(12.126),(4.096),(4.529),(15.810),(9.519),(4.285),(8.390),(12.915),(4.426),(9.155),(16.880),(4.899),(8.390),(5.128),(85.431),(4.181),(4.818),(29.265),(59.999),(70.515),(6.631),(14.641),(3.973),(4.977),(57.247),(21.414),(134.249),(3.884),(4.818),(5.767),(4.395),(4.381),(4.548),(4.152),(22.644),(7.791),(5.608),(5.285),(17.118),(5.911),(16.676),(15.587),(4.109),(4.316),(10.913),(70.515),(21.093),(6.002),(8.448),(7.550),(35.341),(4.124),(4.209),(85.431),(6.519),(5.690),(7.789),(4.027),(15.990),(4.096),(7.694),(13.317),(12.614),(5.266),(7.550),(6.346),(5.767),(19.654),(3.921),(33.269),(10.005),(6.702),(4.001),(25.585),(10.262),(30.762),(11.111),(26.690),(5.478),(4.395),(3.884),(9.755),(6.596),(6.119),(6.558),(17.139),(7.847),(37.472),(6.700),(5.240),(19.379),(17.407),(5.430),(4.411),(26.091),(399.713),(66.264),(5.214),(4.670),(36.380),(4.137),(8.449),(19.379),(5.529),(13.652),(6.155),(37.472),(11.111),(4.272),(6.631),(32.334),(7.230),(33.279),(9.093),(5.529),(17.902),(15.586),(6.742),(5.084),(5.286),(5.407),(7.364),(5.172),(8.963),(27.916),(5.502),(5.407),(4.097),(7.739),(4.363),(37.472),(7.018),(3.872),(52.501),(41.468),(6.283),(9.449),(5.108),(5.128),(4.957),(21.399),(8.888),(4.285),(21.093),(25.013),(6.154),(134.190),(5.712),(7.948),(4.839),(7.495),(63.278),(5.852),(10.707),(4.818),(5.002),(12.019),(5.851),(8.335),(23.547),(593.318),(5.911),(7.948),(7.454),(3.897),(63.360),(10.170),(7.495),(6.190),(4.272),(35.320),(6.060),(63.278),(14.293),(4.671),(9.018),(27.242),(10.920),(7.738),(6.936),(4.529),(13.788),(11.876),(9.376),(17.638),(4.027),(6.519),(6.155),(4.632),(16.425),(66.264),(9.018),(13.792),(23.547),(6.449),(11.876),(4.706),(7.948),(5.110),(18.459),(21.821),(10.170),(36.380),(399.713),(5.631),(4.818),(5.581),(4.722),(16.676),(5.407),(4.688),(3.948),(6.059),(12.619),(13.195),(10.346),(13.794),(3.896),(8.386),(47.836),(35.320),(5.085),(5.883),(8.057),(4.899),(4.510),(4.722),(399.713),(12.771),(57.388),(19.048),(8.390),(7.847),(4.781),(7.183),(19.379),(12.019),(5.128),(6.556),(7.062),(4.395),(9.375),(11.316),(5.357),(6.094),(12.368),(9.093),(12.368),(30.075),(4.839),(4.239),(6.862),(5.266),(10.443),(4.818),(4.066),(4.494),(4.596),(6.188),(7.015),(9.377),(1130.363),(5.084),(5.883),(43.041),(5.310),(5.407),(5.383),(24.437),(13.660),(13.479),(3.936),(4.494),(5.214),(6.596),(4.670),(10.346),(17.902),(16.866),(14.837),(7.794),(18.468),(13.486),(17.638),(5.152),(4.227),(15.586),(4.096),(5.310),(8.636),(5.968),(3.987),(5.042),(6.092),(32.334),(5.285),(10.622),(50.017),(4.272),(34.433),(14.988),(6.120),(12.118),(5.712),(6.317),(30.762),(4.818),(9.754),(6.817),(27.242),(14.283),(8.106),(17.629),(7.791),(9.229),(44.336),(5.108),(54.578),(7.600),(5.966),(79.726),(7.181),(85.447),(11.212),(7.738),(4.688),(12.368),(4.745),(14.103),(6.556),(7.454),(30.762),(8.636),(18.468),(7.598),(7.999),(5.662),(4.197),(6.217),(6.449),(6.556),(134.190),(8.757),(5.529),(5.581),(9.093),(5.608),(6.817),(8.056),(5.197),(11.117),(8.163),(4.209),(46.296),(10.700),(19.052),(5.452),(75.535),(5.286),(15.361),(4.444),(20.664),(10.809),(8.963),(57.247),(14.116),(92.049),(16.880),(4.706),(11.201),(4.272),(54.578),(3.872),(6.897),(20.301),(8.386),(4.670),(4.209),(5.310),(11.753),(42.933),(3.960),(6.283),(4.818),(4.780),(25.585),(8.757),(5.083),(9.912),(13.788),(9.999),(8.163),(33.350)}
https://github.com/keenanpepper/tiptop/blob/main/tiptop.py#L45-L52
