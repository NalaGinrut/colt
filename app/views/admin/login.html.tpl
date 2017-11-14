<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
    <title><%= blog-name %></title>
  </head>

  <body>
    <h1>articles#show</h1>

    <div method="POST" action=<%= action %> >

      <div>
        <textarea name="post-content" rows="20" cols="60">
          <%= content %>
        </textarea>
      </div>
    </div>
    
  </body>
</html>
