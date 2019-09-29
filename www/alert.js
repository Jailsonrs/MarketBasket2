var products = [];
var cart = [];
var prodmrec = [];
var offRate = [];


function addProduct() {
  var productID = document.getElementById("cartAdd").value;
  var ProdRec = document.getElementById("recprod2").value;
  /*var product_desc = document.getElementById("product_desc").value;*/
  var qty = document.getElementById("quantity").value;
  var price = document.getElementById("price").value;
  var desconto = document.getElementById("offR").value;
  var imgSource = document.getElementById("cartAdd").value;


  var productID_rec = document.getElementById("recprod2").value;
  /* var product_desc_rec = document.getElementById("recprod2").value;*/
  var qty_rec = document.getElementById("quantityRec").value;
  var price_rec = document.getElementById("priceRec").value;
  var imgSource_rec = document.getElementById("recprod2").value;

  var newProdRec = {
    product_img: " ",
    product_id: null,
    /*product_desc: null,*/
    product_rec: null,
    product_qty: 0, 
    product_price: 0.00,
    product_off: 0.00, 
  };



  var newProduct = {
    product_img: " ",
    product_id: null,
    /*product_desc: null,*/
    product_rec: null,
    product_qty: 0, 
    product_price: 0.00,
    product_off: 0.00,  
  };


  newProdRec.product_img = imgSource_rec;
  newProdRec.product_id = productID_rec;
  /*newProduct.product_desc = product_desc;*/
  newProdRec.product_rec = ProdRec;
  newProdRec.product_qty = qty_rec;
  newProdRec.product_price = price_rec;
  newProdRec.product_off = desconto;

  newProduct.product_img = imgSource;
  newProduct.product_id = productID;
  /*newProduct.product_desc = product_desc;*/
  newProduct.product_rec = ProdRec;
  newProduct.product_qty = qty;
  newProduct.product_price = price;
  newProduct.product_off = desconto;
  products.push(newProduct);
  var GrandTotal = 0;

  var html = "<table border='1|1' >";
  html += `<tr><td> Imagem do produto </td>`;
  html += `<td> Produto </td>`;
/*  html += "<td> Descrição </td>";*/
  html += "<td>QTD</td>";
  html += "<td>Valor</td>";
  html += `<td>valor com desconto</td>`;
  html += "<td>Total</td>";
  
   for (var i = 0; i < products.length; i++) {
    html += "<tr>";
    html += `<td> <img src = \"${"img"+"/"+products[i].product_img+".jpg"}\" class="zoom" onerror="this.src='img/default.jpg';" height="100" width="120"> </img> </td>`;
    html += "<td>" + products[i].product_id + "</td>";
    /*html += "<td>" + products[i].product_desc + "</td>";*/
    html += "<td>" + products[i].product_qty + "</td>";
    html += "<td>" + products[i].product_price + "</td>";
    html += "<td>" + (products[i].product_price-(products[i].product_price*(products[i].product_off/100))) + "</td>";
    html += "<td><button type='submit' onClick='subtractQuantity(\"" + products[i].product_id + "\", this);'/>Remover unidade</button> &nbsp<button type='submit' onClick='addQuantity(\"" + products[i].product_id + "\", this);'/>Adcionar unidade</button> &nbsp<button type='submit' onClick='deleteProduct(\"" + products[i].product_id + "\", this);'/>Remover item</button></td>";
    html += "</tr>";
    GrandTotal += parseFloat(products[i].product_price) * parseInt(products[i].product_qty);           
    GrandTotal += parseFloat(products[i].product_price) * parseInt(products[i].product_qty);           
  }

  html += "</table>";
  document.getElementById("demo2").innerHTML = html;
  document.getElementById('demo3').innerHTML = "R$ " + GrandTotal;
}


function addProduct2() {
  var productID = document.getElementById("recprod2").value;
  var ProdRec = document.getElementById("recprod2").value;
  /*var product_desc = document.getElementById("product_desc").value;*/
  var qty = document.getElementById("quantityRec").value;
  var price = document.getElementById("priceRec").value;
  var desconto = document.getElementById("offR").value;
    var imgSource = document.getElementById("cartAdd").value;


  var newProduct = {
        product_img: " ",

    product_id: null,
    /*product_desc: null,*/
    product_rec: null,
    product_qty: 0, 
    product_price: 0.00,
    product_off: 0.00,
  };
  newProduct.product_img = imgSource;

  newProduct.product_id = productID;
  /*newProduct.product_desc = product_desc;*/
  newProduct.product_rec = ProdRec;
  newProduct.product_qty = qty;
  newProduct.product_price = price;
   newProduct.product_off = desconto;

  products.push(newProduct);

  var GrandTotal = 0;

  var html = "<table border='1|1' >";
  html += `<tr><td> Imagem do produto </td>`;
  html += `<td> Produto </td>`;
/*  html += "<td> Descrição </td>";*/
  html += "<td>QTD</td>";
  html += "<td>Valor</td>";
  html += `<td>valor com desconto</td>`;
  html += "<td>Total</td>";
  
   for (var i = 0; i < products.length; i++) {
    html += "<tr>";
    html += `<td> <img src = \"${"img"+"/"+products[i].product_img+".jpg"}\" class="zoom" onerror="this.src='img/default.jpg';" height="100" width="120"> </img> </td>`;
    html += "<td>" + products[i].product_id + "</td>";
    /*html += "<td>" + products[i].product_desc + "</td>";*/
    html += "<td>" + products[i].product_qty + "</td>";
    html += "<td>" + products[i].product_price + "</td>";
    html += "<td>" + (products[i].product_price-(products[i].product_price*(products[i].product_off/100))) + "</td>";
    html += "<td><button type='submit' onClick='subtractQuantity(\"" + products[i].product_id + "\", this);'/>Remover unidade</button> &nbsp<button type='submit' onClick='addQuantity(\"" + products[i].product_id + "\", this);'/>Adcionar unidade</button> &nbsp<button type='submit' onClick='deleteProduct(\"" + products[i].product_id + "\", this);'/>Remover item</button></td>";
    html += "</tr>";
    GrandTotal += parseFloat(products[i].product_price) * parseInt(products[i].product_qty);           
  }

  html += "</table>";
  document.getElementById("demo2").innerHTML = html;
  document.getElementById('demo3').innerHTML = "R$ "+GrandTotal;

}

function deleteProduct(product_id, e) {
  e.parentNode.parentNode.parentNode.removeChild(e.parentNode.parentNode);
  for (var i = 0; i < products.length; i++) {
    if (products[i].product_id == product_id) {
                    // DO NOT CHANGE THE 1 HERE
                    products.splice(i, 1);


                  }
                }
              }

             //  function addCart(product_id) {
             //    for (var i = 0; i < products.length; i++) {
             //      if (products[i].product_id == product_id) {
             //        var cartItem = null;
             //        for (var k = 0; k < cart.length; k++) {
             //          if (cart[k].product.product_id == products[i].product_id) {
             //            cartItem = cart[k];
             //            cart[k].product_qty++;
             //            break;
             //          }
             //        }
             //        if (cartItem == null) {

             //          var cartItem = {
             //            product: products[i],
             //            product_qty: products[i].product_qty 
             //          };
             //          cart.push(cartItem);
             //        }
             //      }
             //    }

             // //   renderCartTable();     
             //  }


              // function renderCartTable() {
              //   var html = '';
              //   var ele = document.getElementById("demo2");
                
              //   ele.innerHTML = '';
              //   html += "<table id='tblCart' border='2|2'>";
              //   html += `<tr><td> Produto </td>`;
              //   /*  html += "<td> Descrição </td>";*/
              //   html += "<td>Produto Recomendado</td>";
              //   html += "<td>QTD</td>";
              //   html += "<td>Preço</td>";
              //   html += "<td>Total</td>";
              //   html += "<td>Ação</td></tr>";
              //   var GrandTotal = 0;
              //   for (var i = 0; i < cart.length; i++) {
              //     var imgSource = document.getElementById("cartAdd").value;

              //     html += "<tr>";
              //     html += `<td id=\"${i}\">` + cart[i].product.product_id +"</td>";
              //     /*html += `<td>` + cart[i].product.product_desc + "</td>";*/
              //     html += `<td>` + cart[i].product.product_rec + "</td>";
              //     html += "<td>" + cart[i].product_qty + "</td>";
              //     html += "<td>" + "R$ "  + cart[i].product.product_price + "</td>";
              //     html += "<td>" + parseFloat(cart[i].product.product_price) * parseInt(cart[i].product_qty) + "</td>";
              //     html += "<td><button type='submit' onClick='subtractQuantity(\"" + cart[i].product.product_id + "\", this);'/>Remover unidade</button> &nbsp<button type='submit' onClick='addQuantity(\"" + cart[i].product.product_id + "\", this);'/>Adcionar unidade</button> &nbsp<button type='submit' onClick='removeItem(\"" + cart[i].product.product_id + "\", this);'/>Remover item</button></td>";
              //     html += "</tr>";


              //   }
              //   document.getElementById('demo3').innerHTML = "R$ "+GrandTotal;
              //   html += "</table>"  ;
              //   ele.innerHTML = html;
              // }


              function subtractQuantity(product_id)
              {

                for (var i = 0; i < cart.length; i++) {
                  if (cart[i].product.product_id == product_id) {
                    cart[i].product_qty--;
                  }

                  if (cart[i].product_qty == 0) {
                    cart.splice(i,1);
                  }

                }

              }

              function addQuantity(product_id)
              {

                for (var i = 0; i < cart.length; i++) {
                  if (cart[i].product.product_id == product_id) {
                    cart[i].product_qty++;
                  }  
                }
                    }


              function removeItem(product_id)
              {

                for (var i = 0; i < cart.length; i++) {
                  if (cart[i].product.product_id == product_id) {
                    cart.splice(i,1);
                  }

                }
          
              }





