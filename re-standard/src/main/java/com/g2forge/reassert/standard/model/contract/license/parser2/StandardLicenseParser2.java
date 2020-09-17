package com.g2forge.reassert.standard.model.contract.license.parser2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.helpers.HPrimitive;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.regex.IGroupBuilder;
import com.g2forge.alexandria.regex.IPatternBuilder;
import com.g2forge.alexandria.regex.NamedCharacterClass;
import com.g2forge.alexandria.regex.RegexPattern;
import com.g2forge.alexandria.regex.RegexPattern.Flag;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.standard.model.contract.license.FamilyVersionLicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseFamily;

import lombok.AccessLevel;
import lombok.Getter;

public class StandardLicenseParser2 implements IParser<ILicenseApplied>, ISingleton {
	protected static final StandardLicenseParser2 INSTANCE = new StandardLicenseParser2();

	protected static RegexPattern<LicenseVersion> computeVersionPattern(LicenseVersion.Field field) {
		if (field == null) throw new NullPointerException();

		final RegexPattern<Object> separator = pattern().charClass().character('-').character('.').build().build();

		final IPatternBuilder<Set<RegexPattern.Flag>, LicenseVersion, RegexPattern<?>, RegexPattern<LicenseVersion>> builder = RegexPattern.<LicenseVersion>builder();
		builder.group(LicenseVersion::getMajor, null).digit(10).plus().build();
		if (LicenseVersion.Field.MINOR.compareTo(field) <= 0) {
			final IGroupBuilder<Set<RegexPattern.Flag>, LicenseVersion, RegexPattern<?>, ? extends IPatternBuilder<Set<RegexPattern.Flag>, LicenseVersion, RegexPattern<?>, RegexPattern<LicenseVersion>>> prefix = builder.group().with(separator).group(LicenseVersion::getMinor, null).digit(10).plus().build();
			if (LicenseVersion.Field.PATCH.compareTo(field) <= 0) {
				prefix.group().with(separator).group(LicenseVersion::getPatch, null).digit(10).plus().build().build().opt();
			}
			prefix.build().opt();
		}

		return builder.build(StandardLicenseParser2::createLicenseVersion);
	}

	public static StandardLicenseParser2 create() {
		return INSTANCE;
	}

	protected static LicenseVersion createLicenseVersion(IFunction1<ISerializableFunction1<? super LicenseVersion, ?>, String> fields) {
		final int major = Integer.parseInt(fields.apply(LicenseVersion::getMajor));
		final Integer minor = HPrimitive.parseInteger(fields.apply(LicenseVersion::getMinor));
		final Integer patch = HPrimitive.parseInteger(fields.apply(LicenseVersion::getPatch));
		return new LicenseVersion(major, minor, patch);
	}

	protected static <T> IPatternBuilder<Set<Flag>, T, RegexPattern<?>, RegexPattern<T>> pattern() {
		return RegexPattern.builder(RegexPattern.Flag.CASE_INSENSITIVE);
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final List<RegexPattern<? extends ILicenseFamily>> patterns = computePatterns();

	protected StandardLicenseParser2() {}

	@ReassertLegalOpinion
	protected List<RegexPattern<? extends ILicenseFamily>> computePatterns() {
		final List<RegexPattern<? extends ILicenseFamily>> retVal = new ArrayList<>();

		//final IFunction3<String, LicenseVersion, Boolean, String> computeEnumName = (name, version, isOrLater) -> name.toUpperCase() + Integer.toString(version.getMajor()) + (version.getMinor() != 0 ? Integer.toString(version.getMinor()) : "") + (isOrLater == null ? "" : (isOrLater ? "OrLater" : "Only"));

		final RegexPattern<Object> gap = pattern().charClass().character('-').character('_').named(NamedCharacterClass.Space).build().star().build();
		final RegexPattern<Object> the = pattern().group().with(gap).text("the").build().opt().build();
		final RegexPattern<Object> licenseReq = pattern().with(gap).text("Licen").charClass().character('c').character('s').build().text("e").build(), licenseOpt = pattern().group().with(licenseReq).build().opt().build();
		final RegexPattern<Object> publiclicense = pattern().group().group().with(gap).text("Public").build().opt().with(licenseReq).build().opt().build();

		/*{
			final IConsumer1<IPatternBuilder<?>> prefix = pattern -> pattern.with(the).text("Apache").optional().optional().text("Software").build().with(licenseReq).build();
			final String asf = "ASF";
		
			builder.license(StandardLicense.Apache11).with(prefix).version(1, 1, 0).with(licenseOpt).build();
			builder.license(StandardLicense.Apache11).text(asf).version(1, 1, 0).build();
		
			// Opinion: default to apache 2.0 when version isn't specified
			builder.license(StandardLicense.Apache2).with(prefix).optional().version(2, 0, 0).with(licenseOpt).build().build();
			builder.license(StandardLicense.Apache2).text(asf).version(2, 0, 0).build();
		
			builder.license(StandardLicense.Apache2, text -> {
				final boolean multiline = text.indexOf('\n') >= 0;
				return multiline && Pattern.compile("^\\s+Apache\\s+License\\s+Version\\s+2\\.0").matcher(text).find();
			});
		}
		{
			final String artistic = "Artistic";
			builder.license(StandardLicense.Artistic1).text(artistic).version(1, 0, 0).build();
			builder.license(StandardLicense.Artistic2).text(artistic).version(2, 0, 0).build();
		}
		{
			final IConsumer1<IPatternBuilder<?>> suffix = pattern -> pattern.optional().text(".").build().optional().text("Clause").child(false, false).text("s").build().build();
			final String bsd = "BSD";
			for (int i = 1; i < 6; i++) {
				final String string = Integer.toString(i);
				final StandardLicense actual = StandardLicense.valueOf(bsd + string);
				builder.license(actual).text(bsd).text(string).with(suffix).with(licenseOpt).build();
				builder.license(actual).text(string).with(suffix).text(bsd).with(licenseOpt).build();
			}
		}
		{
			final String cc = "CC", by = "BY", sa = "SA";
			@SuppressWarnings("unchecked")
			final IConsumer1<IPatternBuilder<?>> suffixes = pattern -> pattern.optional().alt(p -> p.text("Unported"), p -> p.text("IT"), p -> p.text("US")).build();
			builder.license(StandardLicense.CC01).text("CC0").with(licenseOpt).optional().version(1, 0, 0).build().optional().text("Universal").with(licenseReq).build().build();
			builder.license(StandardLicense.CCBY3).text(cc).text(by).version(3, 0, 0).with(suffixes).build();
			builder.license(StandardLicense.CCBYSA2).text(cc).text(by).text(sa).version(2, 0, 0).with(suffixes).build();
			builder.license(StandardLicense.CCBYSA3).text(cc).text(by).text(sa).version(3, 0, 0).with(suffixes).build();
		}
		{
			final IConsumer1<IPatternBuilder<?>> cddlLong = pattern -> pattern.text("Common").text("Development").text("and").text("Distribution").with(licenseReq);
			final String cddl = "CDDL";
			for (LicenseVersion version : HCollection.asList(new LicenseVersion(1, 0, 0), new LicenseVersion(1, 1, 0))) {
				final StandardLicense standardLicense = StandardLicense.valueOf(computeEnumName.apply(cddl, version, null));
				builder.license(standardLicense).text(cddl).version(version).build();
				builder.license(standardLicense).with(cddlLong).version(version).build();
			}
		
		}
		{
			final String common = "Common", cpl = "CPL";
			for (LicenseVersion version : HCollection.asList(new LicenseVersion(1, 0, 0))) {
				final StandardLicense standardLicense = StandardLicense.valueOf(computeEnumName.apply(cpl, version, null));
				builder.license(standardLicense).text(common).with(publiclicense).version(version).build();
				builder.license(standardLicense).text(cpl).version(version).build();
			}
		}
		{
			final String eclipse = "Eclipse", epl = "EPL";
			for (LicenseVersion version : HCollection.asList(new LicenseVersion(1, 0, 0), new LicenseVersion(2, 0, 0))) {
				final StandardLicense standardLicense = StandardLicense.valueOf(computeEnumName.apply(epl, version, null));
				builder.license(standardLicense).text(eclipse).with(publiclicense).version(version).build();
				builder.license(standardLicense).text(epl).version(version).build();
			}
		}*/
		{
			// TODO: Switch to group parsing for orlater flag
			final RegexPattern<Object> only = pattern().group().with(gap).text("only").build().opt().with(licenseOpt).build();
			final RegexPattern<Object> orlater = pattern().with(gap).alt(pattern().text("+").build(), pattern().text("or").with(gap).text("later").build()).with(licenseOpt).build();

			final RegexPattern<LicenseVersion> versionPattern = computeVersionPattern(LicenseVersion.Field.MINOR);
			for (StandardLicenseFamily family : new StandardLicenseFamily[] { StandardLicenseFamily.GPL }) {
				final String name = family.getName();

				StandardLicenseParser2.<ILicenseFamily>pattern().with(the).with(versionPattern).build(fields -> {
					// TODO: Find a way to parse misc fields
					final LicenseVersion version = null;//createLicenseVersion(fields);
					return new FamilyVersionLicense(family, version, false);
				});
			}
		}
		/*{
			final IConsumer1<IPatternBuilder<?>> only = pattern -> pattern.optional().text("only").build().with(licenseOpt);
			@SuppressWarnings("unchecked")
			final IConsumer1<IPatternBuilder<?>> orlater = pattern -> pattern.alt(p -> p.text("+"), p -> p.text("or").text("later")).with(licenseOpt);
		
			@SuppressWarnings("unchecked")
			final IConsumer3<String, LicenseVersion, Boolean> creator = (name, version, isOrLater) -> {
				final String shortName = name.replaceAll("([A-Z])[a-zA-Z]+\\s*", "$1");
				final StandardLicense standardLicense = StandardLicense.valueOf(computeEnumName.apply(shortName, version, isOrLater));
		
				final IPatternBuilder<PatternMapBuilder<StandardLicense>> builder0 = builder.license(standardLicense).with(the);
				final IPatternBuilder<PatternMapBuilder<StandardLicense>> builder1 = name.startsWith("GNU") ? builder0 : builder0.optional().text("GNU").build();
				builder1.alt(p -> p.text(name), p -> p.text(shortName)).version(version).with(isOrLater ? orlater : only).build();
			};
		
			for (boolean isOrLater : new boolean[] { false, true }) {
				for (LicenseVersion version : HCollection.asList(new LicenseVersion(1, 0, 0), new LicenseVersion(2, 0, 0), new LicenseVersion(2, 1, 0), new LicenseVersion(3, 0, 0))) {
					creator.accept("General Public License", version, isOrLater);
				}
				for (LicenseVersion version : HCollection.asList(new LicenseVersion(2, 0, 0), new LicenseVersion(2, 1, 0), new LicenseVersion(3, 0, 0))) {
					creator.accept("Lesser General Public License", version, isOrLater);
				}
				for (LicenseVersion version : HCollection.asList(new LicenseVersion(3, 0, 0))) {
					creator.accept("Affero General Public License", version, isOrLater);
				}
				for (LicenseVersion version : HCollection.asList(new LicenseVersion(1, 1, 0), new LicenseVersion(1, 2, 0), new LicenseVersion(1, 3, 0))) {
					creator.accept("GNU Free Documentation License", version, isOrLater);
				}
			}
		}
		{
			builder.license(StandardLicense.MIT).with(the).text("MIT").with(licenseOpt).build();
			builder.license(StandardLicense.MIT).with(the).text("Expat").child(false, false).text("/MIT").build().build();
			builder.license(StandardLicense.MIT).with(the).text("Bouncy").text("Castle").with(licenseReq).build();
		}
		{
			final String mpl = "MPL", mozilla = "Mozilla";
			builder.license(StandardLicense.MPL11).text(mpl).version(1, 1, 0).build();
			builder.license(StandardLicense.MPL11).text(mozilla).with(publiclicense).version(1, 1, 0).build();
			builder.license(StandardLicense.MPL2).text(mpl).version(2, 0, 0).build();
			builder.license(StandardLicense.MPL2).text(mozilla).with(publiclicense).version(2, 0, 0).build();
		}
		{
			builder.license(StandardLicense.OFL11).optional().text("SIL").build().text("OFL").version(1, 1, 0).build();
			builder.license(StandardLicense.OFL11).text("Open").text("Font").with(licenseReq).version(1, 1, 0).build();
		}*/
		{
			//builder.license(StandardLicense.AFL21).text("AFL").version(2, 1, 0).build();
			retVal.add(StandardLicenseParser2.<ILicenseFamily>pattern().text("Beer").with(gap).text("ware").build(fields -> StandardLicense.Beerware));
			/*builder.license(StandardLicense.BSL1).text("Boost").optional().optional().text("Software").build().with(licenseReq).build().optional().version(1, 0, 0).build().build();
			builder.license(StandardLicense.BSL1).text("BSL").optional().version(1, 0, 0).build().build();
			builder.license(StandardLicense.EDL1).text("EDL").version(1, 0, 0).build();*/
			retVal.add(StandardLicenseParser2.<ILicenseFamily>pattern().text("FTL").build(fields -> StandardLicense.FTL));
			/*builder.license(StandardLicense.IndianaExtreme111).text("Indiana").text("University").text("Extreme!").text("Lab").text("Software").with(licenseReq).version(1, 1, 1).build();
			builder.license(StandardLicense.ISC).text("ISC").optional().text("original").build().build();*/
			retVal.add(StandardLicenseParser2.<ILicenseFamily>pattern().text("Owner").build(fields -> StandardLicense.Owner));
			/*builder.license(StandardLicense.Perl5).text("Perl").text("5").with(licenseOpt).optional().text("(").text("GPL").text("or").text("Artistic").text(")").build().build();
			builder.license(StandardLicense.PostgreSQL).text("Postgres").child(false, false).text("ql").build().build();
			builder.license(StandardLicense.PSF2).text("PSF").version(2, 0, 0).build();
			builder.license(StandardLicense.PublicDomain).text("Public").text("Domain").build();
			builder.license(StandardLicense.Python2).text("Python").version(2, 0, 0).build();*/
			retVal.add(StandardLicenseParser2.<ILicenseFamily>pattern().text("WTFPL").build(fields -> StandardLicense.WTFPL));
			retVal.add(StandardLicenseParser2.<ILicenseFamily>pattern().text("ZLIB").with(licenseOpt).build(fields -> StandardLicense.ZLIB));
		}
		return Collections.unmodifiableList(retVal);
	}

	@Override
	public ILicenseApplied parse(String text) {
		if (text == null) return UnspecifiedLicense.create();

		final Set<ILicenseFamily> retVal = new LinkedHashSet<>();
		for (RegexPattern<? extends ILicenseFamily> pattern : getPatterns()) {
			final IOptional<? extends ILicenseFamily> match = pattern.match(text);
			if (!match.isEmpty()) retVal.add(match.get());
		}
		if (retVal.size() == 1) return HCollection.getOne(retVal);
		return new UnknownLicense(text);
	}
}
