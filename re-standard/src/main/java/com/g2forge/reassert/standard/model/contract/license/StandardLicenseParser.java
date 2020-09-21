package com.g2forge.reassert.standard.model.contract.license;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.g2forge.alexandria.java.core.error.UnreachableCodeError;
import com.g2forge.alexandria.java.core.helpers.HPrimitive;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.text.HString;
import com.g2forge.alexandria.parse.IMatch;
import com.g2forge.alexandria.parse.IMatcher;
import com.g2forge.alexandria.parse.IMatcherBuilder;
import com.g2forge.alexandria.parse.IPattern;
import com.g2forge.alexandria.parse.regex.Regex;
import com.g2forge.reassert.contract.model.contract.license.ALicenseParser;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;

import lombok.AccessLevel;
import lombok.Getter;

public class StandardLicenseParser extends ALicenseParser implements ISingleton {
	@ReassertLegalOpinion
	public static class PatternListBuilder extends APatternListBuilder {
		@Override
		public List<IMatcher<? extends ILicenseFamily, ?>> build() {
			final List<IMatcher<? extends ILicenseFamily, ?>> retVal = new ArrayList<>();

			{ // Apache
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().with(the).text("Apache").group(g0 -> g0.group(g1 -> g1.with(gap).text("Software").group(g2 -> g2.with(gap).text("Foundation")).opt()).opt().with(licenseReq)).opt().with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().with(licenseOpt).buildReq(StandardLicenseFamily.Apache::create));
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().with(the).text("ASF").with(licenseOpt).with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().with(licenseOpt).buildReq(StandardLicenseFamily.Apache::create));
				retVal.add(new IMatcher<ILicenseFamily, IPattern>() {
					@Override
					public IOptional<ILicenseFamily> match(String text) {
						final boolean multiline = text.indexOf('\n') >= 0;
						final boolean match = multiline && Pattern.compile("^\\s*Apache\\s+License\\s+Version\\s+2\\.0").matcher(text).find();
						return match ? NullableOptional.of(StandardLicenseFamily.Apache.create(new LicenseVersion(2, 0), false)) : NullableOptional.empty();
					}
				});
			}
			{ // BSD
				final IMatcher<Object, Regex> suffix = pattern().with(gap).group(g -> g.text(".").with(gap)).opt().group(g -> g.text("Clause").text("s").opt()).opt().build();
				retVal.add(APatternListBuilder.<BSDLicense>pattern().text("BSD").with(gap).group(BSDLicense::getClauses, g -> g.digit(10).plus().buildReq(match -> HPrimitive.parseInteger(match.getAsString()))).with(suffix).with(licenseOpt).buildReq(match -> new BSDLicense(match.getAsObject(BSDLicense::getClauses))));
				retVal.add(APatternListBuilder.<BSDLicense>pattern().group(BSDLicense::getClauses, g -> g.digit(10).plus().buildReq(match -> HPrimitive.parseInteger(match.getAsString()))).with(suffix).with(gap).text("BSD").with(licenseOpt).buildReq(match -> new BSDLicense(match.getAsObject(BSDLicense::getClauses))));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().group(g -> {
					@SuppressWarnings("unchecked")
					final IMatcherBuilder<ILicenseFamily, Regex> alt = g.alt(pattern().text("the").build(), pattern().text("revised").build(), pattern().text("new").build());
					alt.with(gap);
				}).opt().text("BSD").group(g -> g.with(gap).text("style")).opt().with(licenseOpt).buildReq(match -> StandardLicenseFamily.BSD));
			}
			{ // Creative Commons
				final IMatcher<CCLicense.Variant, Regex> variants;
				{
					final List<IMatcher<?, Regex>> alts = new ArrayList<>();
					for (CCLicense.Variant variant : CCLicense.Variant.values()) {
						final String abbreviation = variant.getAbbreviation();
						if ((abbreviation != null) && !abbreviation.equals(variant.getText())) alts.add(pattern().text(abbreviation).buildFlag(variant));
						alts.add(pattern().text(variant.getText()).buildFlag(variant));
					}
					variants = APatternListBuilder.<CCLicense.Variant>pattern().alt(alts).build();
				}

				{
					final IMatcherBuilder<CCLicense, Regex> builder = APatternListBuilder.<CCLicense>pattern();
					builder.alt(pattern().text("CC").build(), pattern("Creative Commons"));
					builder.with(gap).group(CCLicense::isZero, g -> g.alt(pattern().text("0").build(), pattern("Zero")).buildReq(IMatch::isMatch)).opt();
					for (CCLicense.Flag flag : CCLicense.Flag.values()) {
						builder.group(flag.getAccessor(), g -> g.with(gap).alt(pattern().text(flag.getAbbreviation()).build(), pattern(flag.getText())).buildReq(IMatch::isMatch)).opt();
					}
					builder.with(CCLicense::getVersion, versionPatternMinor).opt();
					builder.with(gap).with(CCLicense::getVariant, variants).opt();
					retVal.add(builder.with(licenseOpt).build(match -> {
						final Boolean zero = match.getAsObject(CCLicense::isZero);
						final Boolean attribution = match.getAsObject(CCLicense::isAttribution);
						final Boolean nonCommercial = match.getAsObject(CCLicense::isNonCommercial);
						final Boolean noDerivatives = match.getAsObject(CCLicense::isNoDerivatives);
						final Boolean shareAlike = match.getAsObject(CCLicense::isShareAlike);
						final LicenseVersion version = match.getAsObject(CCLicense::getVersion);

						CCLicense.Variant variant = null;
						try {
							variant = match.getAsObject(CCLicense::getVariant);
						} catch (IllegalArgumentException exception) {}

						final CCLicense cc = new CCLicense(zero, attribution, nonCommercial, noDerivatives, shareAlike, version, variant);
						if (!cc.isValid()) NullableOptional.empty();
						return NullableOptional.of(cc.isFamily() ? StandardLicenseFamily.CC : cc);
					}));
				}
			}
			{ // GNU licenses
				final IMatcher<Boolean, Regex> only = APatternListBuilder.<Boolean>pattern().group(g -> g.with(gap).text("only")).opt().build();
				final IMatcher<Boolean, Regex> orlater = APatternListBuilder.<Boolean>pattern().with(gap).group(g -> g.alt(pattern().text("+").build(), pattern().text("or").with(gap).text("later").build()).buildReq(IMatch::isMatch)).build();

				final List<StandardLicenseFamily> gnuLicenseFamilies = Stream.of(StandardLicenseFamily.values()).filter(f -> f.getFamily() == StandardLicenseFamily.GNU).collect(Collectors.toList());
				for (StandardLicenseFamily family : gnuLicenseFamilies) {
					final String name = family.getName().trim();
					final String noprefix = name.startsWith("GNU") ? name.substring(3).trim() : name;

					// Compute the initials while accounting for GFDL requiring the G (who knows what FDL is), and GPL not being GGPL
					final String initials = HString.initials(name);
					final String initialism = family.name().equals(initials) ? initials : HString.initials(noprefix);
					if (!family.name().equals(initialism)) throw new UnreachableCodeError("Developer failed to maintain the GNU license family naming invariants!");

					retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().with(the).group(g -> g.text("GNU")).opt().with(gap).alt(pattern(noprefix), pattern().text(initialism).build()).with(gap).group(g0 -> {
						@SuppressWarnings({ "unchecked", "unused" })
						final IMatcherBuilder<FamilyVersionLicense, Regex> alt = g0.alt(pattern().text("any").build(), pattern().text("ed").build(), APatternListBuilder.<FamilyVersionLicense>pattern().group(g1 -> g1.with(FamilyVersionLicense::getVersion, versionPatternMinor)).build());
					}).opt().alt(only, APatternListBuilder.<FamilyVersionLicense>pattern().with(FamilyVersionLicense::isOrLater, orlater).build()).with(licenseOpt).buildReq(family::create));
				}
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().with(the).group(g -> g.text("GNU")).opt().with(gap).with(pattern("Lesser Public")).with(gap).with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().alt(only, APatternListBuilder.<FamilyVersionLicense>pattern().with(FamilyVersionLicense::isOrLater, orlater).build()).with(licenseOpt).buildReq(StandardLicenseFamily.LGPL::create));
			}
			{ // MMIT
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().with(the).text("MIT").with(licenseOpt).buildReq(match -> StandardLicense.MIT));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().with(the).text("Expat").group(g -> g.text("/MIT")).opt().buildReq(match -> StandardLicense.MIT));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().with(the).text("Bouncy").with(gap).text("Castle").with(licenseReq).buildReq(match -> StandardLicense.MIT));
			}
			{ // Misc versioned
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().with(the).text("Artistic").group(g -> g.with(gap).alt(licenseReq, pattern().text("dist").build())).opt().with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().buildReq(StandardLicenseFamily.Artistic::create));
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().text("Boost").with(licenseOpt).with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().buildReq(StandardLicenseFamily.BSL::create));
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().with(pattern(HString.stripSuffix(StandardLicenseFamily.IndianaExtreme.getName(), "License").trim())).with(licenseOpt).with(FamilyVersionLicense::getVersion, versionPatternPatch).buildReq(StandardLicenseFamily.IndianaExtreme::create));
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().group(g -> g.text("SIL").with(gap)).opt().text("OFL").with(FamilyVersionLicense::getVersion, versionPatternMinor).buildReq(StandardLicenseFamily.OFL::create));
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().text("Open").with(gap).text("Font").with(licenseOpt).with(FamilyVersionLicense::getVersion, versionPatternMinor).buildReq(StandardLicenseFamily.OFL::create));
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().text("PSF").with(FamilyVersionLicense::getVersion, versionPatternMinor).buildReq(StandardLicenseFamily.PSF::create));
				retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().text("Python").with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().buildReq(StandardLicenseFamily.Python::create));
			}
			{ // Misc versioned with patterns
				for (StandardLicenseFamily family : new StandardLicenseFamily[] { StandardLicenseFamily.AFL, StandardLicenseFamily.BSL, StandardLicenseFamily.EDL, StandardLicenseFamily.CDDL }) {
					retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().with(pattern(HString.stripSuffix(family.getName(), "License").trim())).with(licenseOpt).with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().buildReq(family::create));
					retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().text(HString.initials(family.getName())).with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().buildReq(family::create));
				}
				for (StandardLicenseFamily family : new StandardLicenseFamily[] { StandardLicenseFamily.CPL, StandardLicenseFamily.EPL, StandardLicenseFamily.MPL }) {
					final String word = HString.stripSuffix(family.getName(), "Public License").trim();
					retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().text(word).with(publiclicense).with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().buildReq(family::create));
					retVal.add(APatternListBuilder.<FamilyVersionLicense>pattern().text(HString.initials(family.getName())).with(FamilyVersionLicense::getVersion, versionPatternMinor).opt().buildReq(family::create));
				}
			}
			{ // Unversioned
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().with(pattern("Beer ware")).buildReq(match -> StandardLicense.Beerware));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().text("FTL").buildReq(match -> StandardLicense.FTL));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().text("ISC").group(g -> g.with(gap).text("original")).opt().buildReq(match -> StandardLicense.ISC));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().text("Owner").buildReq(match -> StandardLicense.Owner));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().text("Perl").with(gap).text("5").with(licenseOpt).group(g -> g.with(gap).text("(").with(gap).text("GPL").with(gap).text("or").with(gap).text("Artistic").with(gap).text(")")).opt().buildReq(match -> StandardLicense.Perl5));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().text("Postgres").group(g -> g.text("ql")).opt().buildReq(match -> StandardLicense.PostgreSQL));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().with(pattern("Public Domain")).buildReq(match -> StandardLicense.PublicDomain));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().text("WTFPL").buildReq(match -> StandardLicense.WTFPL));
				retVal.add(APatternListBuilder.<ILicenseFamily>pattern().text("ZLIB").with(licenseOpt).buildReq(match -> StandardLicense.ZLIB));
			}
			return Collections.unmodifiableList(retVal);
		}
	}

	protected static final StandardLicenseParser INSTANCE = new StandardLicenseParser();

	public static StandardLicenseParser create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final List<IMatcher<? extends ILicenseFamily, ?>> matchers = new PatternListBuilder().build();

	protected StandardLicenseParser() {}
}
