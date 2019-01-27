package java8news;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CollectorTest {

	public static void test() {
//		toOrderSet().forEach(Artist::printMembers);
//		System.out.println(averageNumberOfTracks());
//		biggestGroup().ifPresent(Artist::printMembers);
		printBandsAndSolo(dataPartitioning());
	}

	private static Stream<Artist> getArtists() {
		Artist artist1 = new Artist();
		artist1.setMembers(Collections.singletonList("ldc"));
		Artist artist2 = new Artist();
		artist2.setMembers(Arrays.asList("tom1", "tom2", "tom3", "tom4"));
		Artist artist3 = new Artist();
		artist3.setMembers(Arrays.asList("Jerry1", "Jerry2", "Jerry3"));
		Artist artist4 = new Artist();
		artist4.setMembers(Collections.singletonList("Cre-32NT"));
		return Stream.of(artist1, artist2, artist3, artist4);
	}

	private static Stream<Album> getAlbums() {
		Album album1 = new Album();
		album1.setMainMusician("Cre-32NT");
		album1.setTracks(Arrays.asList("enSong1", "enSong2", "enSong3"));
		Album album2 = new Album();
		album2.setMainMusician("Cre-32NT");
		album2.setTracks(Arrays.asList("chSong1", "chSong2", "chSong3"));
		Album album3 = new Album();
		album3.setMainMusician("Halozy");
		album3.setTracks(Arrays.asList("jpSong1", "jpSong2", "jpSong3"));
		Album album4 = new Album();
		album4.setMainMusician("xi");
		album4.setTracks(Arrays.asList("pureMusic1", "pureMusic2", "pureMusic3", "pureMusic4"));
		return Stream.of(new Album(), album2, album3);
	}

	/*
			由于 Predicate 对象的 test 方法反悔的是 isSolo()
			所以 get(true) 可以得到独唱歌手，get(false) 可以得到乐队
	 */
	private static void printBandsAndSolo(Map<Boolean, List<Artist>> artistMap) {
		artistMap.get(true).forEach(Artist::printMembers);
		artistMap.get(false).forEach(Artist::printMembers);
	}

	/*
			toCollection(Collection::new) 可以将一个集合转换成其它集合
			返回结果可以是任何 Collection 的子接口或者实现了该接口的抽象类及子类
			返回类型并不限于 Set，还可以是 List, Map
	 */
	private static Collection<Artist> toOrderSet() {
//        return getArtists().collect(Collectors.toCollection(ArrayList::new));
		return getArtists().collect(Collectors.toCollection(HashSet::new));
	}

	private static Double averageNumberOfTracks() {
		return getAlbums().collect(Collectors.averagingDouble(Album::getTracksSize));
	}

	private static Optional<Artist> biggestGroup() {
		return getArtists().max(Comparator.comparing(Artist::getMemberCount));
	}

	/*
			收集器 predicateBy 接收一个 Predicate 对象
			需要做的是重写它的 test 方法并返回一个 boolean 值
			predicateBy 会根据 boolean 值对流进行筛选并放到一个 List 里
			最后返回一个 Map<Boolean, List<T>>
	 */
	private static Map<Boolean, List<Artist>> dataPartitioning() {
		return getArtists().collect(Collectors.partitioningBy(Artist::isSolo));
	}
}
