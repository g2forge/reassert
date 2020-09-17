package com.g2forge.reassert.standard.model.contract.license.parser.v2;

import org.junit.Test;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.test.HAssert;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public class TestRegexPattern {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class Version {
		public static Version createVersion(IFunction1<ISerializableFunction1<? super TestRegexPattern.Version, ?>, String> fields) {
			final int major = Integer.parseInt(fields.apply(TestRegexPattern.Version::getMajor));
			final Integer minor = HPrimitive.parseInteger(fields.apply(TestRegexPattern.Version::getMinor));
			final Integer patch = HPrimitive.parseInteger(fields.apply(TestRegexPattern.Version::getPatch));
			return new Version(major, minor, patch);
		}

		protected final int major;

		protected final Integer minor;

		protected final Integer patch;
	}

	protected static final RegexPattern<Version> version = computeVersion();

	protected static RegexPattern<Version> computeVersion() {
		final IPatternBuilder<Void, Version, RegexPattern<?>, RegexPattern<Version>> builder = RegexPattern.<Version>builder();
		builder.group(Version::getMajor, null).text("0").build();
		builder.group().text(".").group(Version::getMinor, null).text("1").build().group().text(".").group(Version::getPatch, null).text("2").build().build().opt().build().opt();
		return builder.build(TestRegexPattern.Version::createVersion);
	}

	@Test
	public void alt() {
		final RegexPattern<?> pattern = RegexPattern.builder().alt(RegexPattern.builder().text("a").build(), RegexPattern.builder().text("b").build()).build();
		HAssert.assertFalse(pattern.match("a").isEmpty());
		HAssert.assertFalse(pattern.match("b").isEmpty());
	}

	@Test
	public void groupNullArguments() {
		RegexPattern.builder().group();
	}

	@Test
	public void groupOptional() {
		final RegexPattern<?> pattern = RegexPattern.builder().text("a").group().text("b").build().opt().build();
		HAssert.assertFalse(pattern.match("a").isEmpty());
		HAssert.assertFalse(pattern.match("ab").isEmpty());
	}

	@Test
	public void groupRequired() {
		final RegexPattern<?> pattern = RegexPattern.builder().text("a").group().text("b").build().build();
		HAssert.assertTrue(pattern.match("a").isEmpty());
		HAssert.assertFalse(pattern.match("ab").isEmpty());
	}

	@Test
	public void match() {
		HAssert.assertFalse(RegexPattern.builder().text("a").build().match("a").isEmpty());
	}

	@Test
	public void nonmatch() {
		HAssert.assertTrue(RegexPattern.builder().text("a").build().match("").isEmpty());
	}

	@Test
	public void plus() {
		final RegexPattern<?> pattern = RegexPattern.builder().text("a").text("b").plus().build();
		HAssert.assertTrue(pattern.match("a").isEmpty());
		HAssert.assertFalse(pattern.match("ab").isEmpty());
		HAssert.assertFalse(pattern.match("abb").isEmpty());
	}

	@Test
	public void star() {
		final RegexPattern<?> pattern = RegexPattern.builder().text("a").text("b").star().build();
		HAssert.assertFalse(pattern.match("a").isEmpty());
		HAssert.assertFalse(pattern.match("ab").isEmpty());
		HAssert.assertFalse(pattern.match("abb").isEmpty());
	}

	@Test
	public void versionMajor() {
		HAssert.assertEquals(new Version(0, null, null), version.match("0").get());
	}

	@Test
	public void versionMinor() {
		HAssert.assertEquals(new Version(0, 1, null), version.match("0.1").get());
	}

	@Test
	public void versionPatch() {
		HAssert.assertEquals(new Version(0, 1, 2), version.match("0.1.2").get());
	}

	@Test
	public void versionWith() {
		final RegexPattern<Version> pattern = RegexPattern.<Version>builder().with(version).build(TestRegexPattern.Version::createVersion);
		HAssert.assertEquals(new Version(0, 1, null), pattern.match("0.1").get());
	}

	@Test
	public void with() {
		final RegexPattern<?> a = RegexPattern.builder().text("a").build();
		final RegexPattern<?> pattern = RegexPattern.builder().text("0").with(a).text("1").build();
		HAssert.assertFalse(pattern.match("0a1").isEmpty());
	}
}
