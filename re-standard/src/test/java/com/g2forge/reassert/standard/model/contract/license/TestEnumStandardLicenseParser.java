package com.g2forge.reassert.standard.model.contract.license;

import java.util.List;

import org.junit.runners.Parameterized.Parameters;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public class TestEnumStandardLicenseParser extends ATestStandardLicenseParser {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class TestCase implements ITestCase {
		protected final String license;

		protected final String text;

		protected final String purpose;

		@Override
		public ILicenseApplied getLicenseApplied() {
			if (getLicense().isEmpty()) return new UnknownLicense(getText());
			else {
				try {
					return StandardLicense.valueOfSPDX(getLicense());
				} catch (IllegalArgumentException e0) {
					try {
						return StandardLicense.valueOf(getLicense());
					} catch (IllegalArgumentException e1) {
						e1.addSuppressed(e0);
						throw e1;
					}
				}
			}
		}
	}

	@Parameters(name = "{1}")
	public static List<Object[]> computeTestParameters() {
		return ATestStandardLicenseParser.computeTestParameters(TestCase.class, "enum license texts.csv");
	}
}
