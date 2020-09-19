package com.g2forge.reassert.standard.model.contract.license;

import java.util.List;

import org.junit.runners.Parameterized.Parameters;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.standard.model.contract.license.CCLicense;
import com.g2forge.reassert.standard.model.contract.license.CCLicense.Variant;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public class TestCCStandardLicenseParser extends ATestStandardLicenseParser {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class TestCase implements ITestCase {
		protected final boolean zero;
		
		protected final boolean attribution;

		protected final boolean nonCommercial;

		protected final boolean noDerivatives;

		protected final boolean shareAlike;

		protected final String version;

		protected final String variant;

		protected final String text;

		protected final String purpose;

		@Override
		public ILicenseApplied getLicenseApplied() {
			final String versionString = getVersion();
			final LicenseVersion version = ((versionString == null) || (versionString.length() == 0)) ? null : new LicenseVersion(versionString);

			final String variantString = getVariant();
			final Variant variant = ((variantString == null) || variantString.isEmpty()) ? null : Variant.valueOf(variantString);

			return new CCLicense(isZero(), isAttribution(), isNonCommercial(), isNoDerivatives(), isShareAlike(), version, variant);
		}
	}

	@Parameters(name = "{1}")
	public static List<Object[]> computeTestParameters() {
		return ATestStandardLicenseParser.computeTestParameters(TestCase.class, "cc license texts.csv");
	}
}
