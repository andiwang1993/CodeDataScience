% S�parateurs � Vaste Marge
% Cas lin�aire, non lin�aire, marges "souples"
% structure du code: c'est juste une proposition, � vous de voir

clear all
close all


 load 'data2' X lab     % chargement des donn�es 

  
% bornes pour le dessin 2D
xmin=min(X(1,:));
ymin=min(X(2,:));
xmax=max(X(1,:));
ymax=max(X(2,:));

na=length(X); % nombre de points (d'apprentissage)

% dessin des points dans R^2

for i=1:na
    if lab(i)==1
        plot(X(1,i),X(2,i),'o','linewidth',2)
        hold on
    else
        plot(X(1,i),X(2,i),'x','linewidth',2,'markersize',12)
        hold on
    end
end
axis([xmin xmax ymin ymax])
grid
axis equal
hold on

pause 

% assemblage matrice de la forme quadratique du probl�me dual
A=zeros(na); %initialisation

% A vous de construire A


% gradient � pas constant pour probl�me dual
alph0=0.5*ones(na,1); % point de d�part (0, ou autre): r�glable
pasgrad=5e-3;         % pas du gradient : parametre r�glable
u=ones(na,1);         % vecteur de 1
crit_arret=1;         % initialisation critere d'arret
npas=0;               % comptage du nombre de pas
npas_max=100000;      % garde-fou convergence (si �a ne converge pas...)
epsi=1e-5;            % seuil convergence

while and(crit_arret>epsi,npas<npas_max)

    % ici la boucle de gradient � pas constant
    % ne pas oublier les projections....
    crit_arret=epsi/2 ; % je le mets juste pour �viter la boucle infinie
                        % tant que le code n'est pas �crit
end

% Ce n'est pas fini! Il faut construire la fonction de decision <w,x> + b
% tracer ses isovaleurs 0 (la droite de partage) et +1 et -1 : les marges
% donc calculer b d'abord, et <w,x> pour tout x 

% comment calculer b? id�e: en se servant des "points supports" x_s pour
% lesquels <w,x_s> +b = + ou - 1
% comment trouver les points support? Qu'est ce qui le caract�rise?? 

Points_support=find(alph>epsi)' % points supports = contraintes actives...alph>0
                                % aux erreurs num�riques pr�s

% Calcul de b (on le calcule pour chaque pt support, puis on moyenne)
% on devrait v�rifier que chaque pt support fournit le m�me b (aux erreurs
% num�riques pr�s, c'est pouquoi on moyenne)
nb_sup=0;
for i=Points_support  % i d�crit le tableau des indices des points supports
   %calcul de b pour chaque point support
end


%calcul de w et trac� de la droite et de ses marges

% une m�thode, qui parait lourde mais qui est la bonne dans le cas de 
% s�paration non lin�aire, est de construire la fonction de d�cision
% sur une grille et d'en tracer les trois isovaleurs avec une fonction
% Matlab

% fonction de d�cision <w,x> + b 


xp=xmin:0.2:xmax;   % cr�ation d'une grille 
yp=ymin:0.2:ymax;
npx=length(xp);
npy=length(yp);
for i=1:npx
    for j=1:npy
        % calcul de <w,x> + b sur une grille
       % V(i,j)= .....   
    end
end
hold on
contour(xp,yp,V',[-1 0 1],'linewidth',2,'color','r')% dessin des contours
axis([xmin xmax ymin ymax])
title(' Suport Vector Machine')
grid



