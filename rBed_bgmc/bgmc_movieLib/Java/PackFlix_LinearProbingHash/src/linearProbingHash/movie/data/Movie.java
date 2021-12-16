package linearProbingHash.movie.data;

public class Movie implements Comparable<Movie> {

	private String id;
	private String title;
	private int year;
	private String[] genre;
	private int runtime;
	
	
	public Movie(String id, String title, int year, String[] genre, int runtime) {
		setId(id);
		setTitle(title);
		setYear(year);
		setGenre(genre);
		setRuntime(runtime);
	}


	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}


	/**
	 * @param id the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}


	/**
	 * @return the title
	 */
	public String getTitle() {
		return title;
	}


	/**
	 * @param title the title to set
	 */
	public void setTitle(String title) {
		this.title = title;
	}


	/**
	 * @return the year
	 */
	public int getYear() {
		return year;
	}


	/**
	 * @param year the year to set
	 */
	public void setYear(int year) {
		this.year = year;
	}


	/**
	 * @return the genre
	 */
	public String[] getGenre() {
		return genre;
	}


	/**
	 * @param genre the genre to set
	 */
	public void setGenre(String[] genre) {
		this.genre = genre;
	}


	/**
	 * @return the runtime
	 */
	public int getRuntime() {
		return runtime;
	}


	/**
	 * @param runtime the runtime to set
	 */
	public void setRuntime(int runtime) {
		this.runtime = runtime;
	}
	
	public int compareTo(Movie other) {
		return this.id.compareTo(other.id);
	}
		  
	public int hashCode() {
		int prime = 31;
		int result = 1;
		result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
		return result;
	}
	  
	public boolean equals(Object obj) {
		if (this == obj)
			return true; 
		if (!(obj instanceof Movie))
			return false; 
		Movie other = (Movie)obj;
		if (this.id == null) {
			if (other.id != null)
				return false; 
		} else if (!this.id.equals(other.id)) {
			return false;
		} 
		return true;
	}
	
}
