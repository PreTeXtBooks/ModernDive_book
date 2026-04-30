var ptx_lunr_search_style = "textbook";
var ptx_lunr_docs = [
{
  "id": "book-id-here",
  "level": "1",
  "url": "book-id-here.html",
  "type": "Book",
  "number": "",
  "title": "Title",
  "body": "  Title  Subtitle                                                    "
}
]

var ptx_lunr_idx = lunr(function () {
  this.ref('id')
  this.field('title')
  this.field('body')
  this.metadataWhitelist = ['position']

  ptx_lunr_docs.forEach(function (doc) {
    this.add(doc)
  }, this)
})
